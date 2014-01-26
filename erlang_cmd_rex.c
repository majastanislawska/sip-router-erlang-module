#include <sys/types.h>
#include <unistd.h>
#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
#include "../../mod_fix.h"

int fixup_cmd_erlang_rex(void** param, int param_no){
	if (param_no <= 3)
		return fixup_spve_null(param, 1);
	if (param_no == 5) {
		return fixup_pvar_null(param, 1);
	}
	if (param_no == 4) {
		pv_elem_t *model=NULL;
		str s;
		s.s = (char*)(*param);
		s.len = strlen(s.s);
		if(s.len==0) {
			LM_ERR("cmd_erlang_rex: param %d is empty string! please use erlang empty list [].\n", param_no);
			return -1;
		}
		if(pv_parse_format(&s ,&model) || model==NULL) {
			LM_ERR("cmd_erlang_rex: wrong format [%s] for value param!\n", s.s);
			return -1;
		}
		*param = (void*)model;
		return 0;
	}
	LM_ERR("erlang_rex takes exactly 5 parameters.\n");
	return -1;
}

int cmd_erlang_rex(struct sip_msg* msg, char *cn , char *mo, char *fu, char *ar, char *_ret_pv) {
#define AVP_PRINTBUF_SIZE 1024
	static char printbuf[AVP_PRINTBUF_SIZE];
	int printbuf_len, bytessent;
	ei_x_buff argbuf;
	pv_spec_t *ret_pv;
	struct nodes_list* node;
	struct erlang_cmd *erl_cmd;
	erlang_pid erl_pid;
	str conname, regproc;
	str mod, fun;
	int retcode = -1;

	if(msg==NULL) {
	    LM_ERR("cmd_erlang_rex: received null msg\n");
	    return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)cn, &conname)<0) {
	    LM_ERR("cmd_erlang_rex: cannot get the connection name\n");
	    return -1;
	}
	for(node=nodes_lst;node;node=node->next) {
		LM_DBG("matching %s with %.*s\n",node->name,conname.len,conname.s);
		if(strcmp(node->name, conname.s)==0) break;
	}
	if(node==0){
		LM_ERR("cmd_erlang_rex: no such connection %.*s\n",conname.len,conname.s);
		return -1;
	}

	if(fixup_get_svalue(msg, (gparam_p)mo, &mod)<0) {
	    LM_ERR("cmd_erlang_rex: cannot get the mod name\n");
	    return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)fu, &fun)<0) {
	    LM_ERR("cmd_erlang_rex: cannot get the fun name\n");
	    return -1;
	}

	printbuf_len = AVP_PRINTBUF_SIZE-1;
	if(pv_printf(msg, (pv_elem_p)ar, printbuf, &printbuf_len)<0 || printbuf_len<=0)
	{
		LM_ERR("erlang_cmd_rex: cannot expand args expression.\n");
		return -1;
	}

	erl_cmd=shm_malloc(sizeof(struct erlang_cmd));
	if(!erl_cmd) {
	    LM_ERR("no shm");
	    return -1;
	}
	memset(erl_cmd,0,sizeof(struct erlang_cmd));

	erl_cmd->erlbuf=shm_malloc(AVP_PRINTBUF_SIZE);
	if (!erl_cmd->erlbuf) {
	    LM_ERR("no shm memory\n\n");
	    goto error;
	}

	ret_pv = (pv_spec_t*)shm_malloc(sizeof(pv_spec_t));
	if (!ret_pv) {
	    LM_ERR("no shm memory\n\n");
	    goto error;
	}
	memcpy(ret_pv, (pv_spec_t *)_ret_pv, sizeof(pv_spec_t));

	if(lock_init(&(erl_cmd->lock))==NULL) {
	    LM_ERR("cannot init the lock\n");
	    goto error;
	}
	regproc.s="rex";
	regproc.len=3;
	erl_cmd->reg_name=shm_strdup(&regproc);
	if (!erl_cmd->reg_name) {
	    LM_ERR("no shm memory\n\n");
	    goto error;
	}
	LM_DBG("cmd_erlang_rex:  %.*s rex %.*s\n",conname.len,conname.s,
			printbuf_len,printbuf);

	erl_cmd->cmd=ERLANG_REX;
	erl_cmd->node=node;
	argbuf.buff=erl_cmd->erlbuf;
	argbuf.buffsz=AVP_PRINTBUF_SIZE-1;
	argbuf.index=0;
	ei_x_encode_version(&argbuf);
	//User passed term will be second element of tuple, first is our (hacked) erlang pid 
	ei_x_encode_tuple_header(&argbuf, 2);
	// we are hacking erlang pid so we can have worker system pid here
	memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
	erl_pid.num=getpid();
	ei_x_encode_pid(&argbuf, &erl_pid);
	erl_cmd->num=erl_pid.num;
	erl_cmd->serial=erl_pid.serial;
	//so much for pid, now encode rex call tuple {call, mod, fun, arg, user}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"call");
	ei_x_encode_atom_len(&argbuf, mod.s, mod.len);
	ei_x_encode_atom_len(&argbuf, fun.s, fun.len);
	if(ei_x_format_wo_ver(&argbuf, printbuf)!=0) {
		LM_ERR("cannot fromat erlang binary from arg string\n");
		goto error;
	}
	ei_x_encode_atom(&argbuf,"user");
	erl_cmd->erlbuf_len=argbuf.index;

	lock_get(&(erl_cmd->lock));
	bytessent=write(pipe_fds[1], &erl_cmd, sizeof(erl_cmd));
	LM_DBG("cmd_erlang_call: locked, sent %d  %d %s %d %p %p, waiting for release\n",bytessent,
			erl_cmd->cmd,erl_cmd->reg_name,erl_cmd->erlbuf_len,erl_cmd->erlbuf, erl_cmd);
	lock_get(&(erl_cmd->lock));
	LM_DBG("after lock\n");
	if (erl_cmd->retcode <0) {
	    retcode=erl_cmd->retcode;
	    LM_DBG("cmd_erlang_call: failed %d\n",retcode);
	    goto error;
	}
	//reuse
	argbuf.buff=erl_cmd->erlbuf;
	argbuf.buffsz=erl_cmd->erlbuf_len;
	argbuf.index=erl_cmd->decode_index;
	fill_retpv(ret_pv,&argbuf,&(argbuf.index));
	retcode=1;
error:
	if(erl_cmd) {
	    if(ret_pv) shm_free(ret_pv);
	    if(erl_cmd->erlbuf) shm_free(erl_cmd->erlbuf);
	    if(erl_cmd->reg_name) shm_free(erl_cmd->reg_name);
	    shm_free(erl_cmd);
	}
	return retcode;
}
