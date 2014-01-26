#include <sys/types.h>
#include <unistd.h>
#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
#include "../../mod_fix.h"

int fixup_cmd_erlang_info(void** param, int param_no){
	if (param_no <= 2)
		return fixup_spve_null(param, 1);
	if (param_no == 3) {
		pv_elem_t *model=NULL;
		str s;
		s.s = (char*)(*param);
		s.len = strlen(s.s);
		if(s.len==0) {
			LM_ERR("cmd_erlang_info: param %d is empty string!\n", param_no);
			return -1;
		}
		if(pv_parse_format(&s ,&model) || model==NULL) {
			LM_ERR("cmd_erlang_info: wrong format [%s] for value param!\n", s.s);
			return -1;
		}
		*param = (void*)model;
		return 0;
	}
	LM_ERR("erlang_info takes exactly 3 parameters.\n");
	return -1;
}

int cmd_erlang_info(struct sip_msg* msg, char *cn, char *rp, char *ar) {
#define AVP_PRINTBUF_SIZE 1024
	static char printbuf[AVP_PRINTBUF_SIZE];
	int printbuf_len, bytessent;
	ei_x_buff argbuf;
	struct nodes_list* node;
	struct erlang_cmd *erl_cmd;
	erlang_pid erl_pid;
	str conname, regproc;
	int retcode = -1;

	if(msg==NULL) {
	    LM_ERR("cmd_erlang_info: received null msg\n");
	    return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)cn, &conname)<0) {
	    LM_ERR("cmd_erlang_info: cannot get the connection name\n");
	    return -1;
	}
	for(node=nodes_lst;node;node=node->next) {
		LM_DBG("cmd_erlang_info: matching %s with %.*s\n",node->name,conname.len,conname.s);
		if(strcmp(node->name, conname.s)==0) break;
	}
	if(node==0){
		LM_ERR("cmd_erlang_info: no such connection %.*s\n",conname.len,conname.s);
		return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)rp, &regproc)<0) {
	    LM_ERR("cmd_erlang_info: cannot get the regproc name\n");
	    return -1;
	}
	printbuf_len = AVP_PRINTBUF_SIZE-1;
	if(pv_printf(msg, (pv_elem_p)ar, printbuf, &printbuf_len)<0 || printbuf_len<=0)
	{
		LM_ERR("cmd_erlang_info: cannot expand erlang message body\n");
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

	if(lock_init(&(erl_cmd->lock))==NULL) {
	    LM_ERR("cannot init the lock\n");
	    goto error;
	}
	erl_cmd->reg_name=shm_strdup(&regproc);
	if (!erl_cmd->reg_name) {
	    LM_ERR("no shm memory\n\n");
	    goto error;
	}

	LM_DBG("cmd_erlang_info:  %.*s %.*s %.*s\n",conname.len,conname.s,
			regproc.len,regproc.s, printbuf_len,printbuf);

	erl_cmd->cmd=ERLANG_INFO;
	erl_cmd->node=node;
	argbuf.buff=erl_cmd->erlbuf;
	argbuf.buffsz=AVP_PRINTBUF_SIZE-1;
	argbuf.index=0;
	ei_x_encode_version(&argbuf);
	//User passed term will be sedond element of tuple, first is our erlang pid 
	ei_x_encode_tuple_header(&argbuf, 2);
	// we are hacking erlang pid so we can have worker system pid here
	memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
	erl_pid.num=getpid();
	ei_x_encode_pid(&argbuf, &erl_pid);

	//so much for pid, now encode term
	if(ei_x_format_wo_ver(&argbuf, printbuf)!=0) {
		LM_ERR("cannot fromat erlang binary from arg string\n");
		goto error;
	}
//	ei_x_encode_atom(&argbuf,"user");
	erl_cmd->erlbuf_len=argbuf.index;

	lock_get(&(erl_cmd->lock));
	bytessent=write(pipe_fds[1], &erl_cmd, sizeof(erl_cmd));
	LM_DBG("cmd_erlang_info: locked, sent %d  %d %s %d %p %p, waiting for release\n",bytessent,
			erl_cmd->cmd,erl_cmd->reg_name,erl_cmd->erlbuf_len,erl_cmd->erlbuf, erl_cmd);
	lock_get(&(erl_cmd->lock));
	LM_DBG("after lock\n");
	if (erl_cmd->retcode <0) {
	    retcode=erl_cmd->retcode;
	    LM_DBG("cmd_erlang_info: failed %d\n",retcode);
	    goto error;
	}

	retcode=1;
error:
	if(erl_cmd) {
	    if(erl_cmd->erlbuf) shm_free(erl_cmd->erlbuf);
	    if(erl_cmd->reg_name) shm_free(erl_cmd->reg_name);
	    shm_free(erl_cmd);
	}
	return retcode;
}
