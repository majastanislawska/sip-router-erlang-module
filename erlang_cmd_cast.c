#include <sys/types.h>
#include <unistd.h>
#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
#include "../../mod_fix.h"

int fixup_cmd_erlang_cast(void** param, int param_no){
	if (param_no <= 2)
		return fixup_spve_null(param, 1);
	if (param_no == 3) {
		pv_elem_t *model=NULL;
		str s;
		s.s = (char*)(*param);
		s.len = strlen(s.s);
		if(s.len==0) {
			LM_ERR("param %d is empty string!\n", param_no);
			return -1;
		}
		if(pv_parse_format(&s ,&model) || model==NULL) {
			LM_ERR("wrong format [%s] for value param!\n", s.s);
			return -1;
		}
		*param = (void*)model;
		return 0;
	}
	LM_ERR("erlang_cast takes exactly 3 parameters.\n");
	return -1;
}

int cmd_erlang_cast(struct sip_msg* msg, char* cn, char* rp, char* et) {
#define AVP_PRINTBUF_SIZE 1024
	static char printbuf[AVP_PRINTBUF_SIZE];
	int printbuf_len, bytessent;
	ei_x_buff erlbuf;
	struct nodes_list* node;
	struct erlang_cmd erl_cmd;
	erlang_pid erl_pid;
	str conname;
	str regproc;
//	str erlterm;

	if(msg==NULL) {
		LM_ERR("received null msg\n");
		return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)cn, &conname)<0) {
		    LM_ERR("cmd_erlang_cast: cannot get the connection name\n");
		    return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)rp, &regproc)<0) {
		    LM_ERR("cmd_erlang_cast: cannot get the regproc name\n");
		    return -1;
	}
//	if(fixup_get_svalue(msg, (pv_elem_t)et, &erlterm)<0) {
//		    LM_ERR("cmd_erlang_cast: cannot get the erlmessage\n");
//		    return -1;
//	}
	printbuf_len = AVP_PRINTBUF_SIZE-1;
	if(pv_printf(msg, (pv_elem_t *)et, printbuf, &printbuf_len)<0 || printbuf_len<=0)
	{
		LM_ERR("cmd_erlang_cast: cannot expand erlang message body\n");
		return -1;
	}
	LM_ERR("cmd_erlang_cast %.*s %.*s %.*s\n",conname.len,conname.s,regproc.len,regproc.s,printbuf_len,printbuf);
	for(node=nodes_lst;node;node=node->next) {
		LM_DBG("matching %s with %.*s\n",node->name,conname.len,conname.s);
		if(strcmp(node->name, conname.s)==0) break;
	}
	if(node==0){ 
		LM_ERR("cmd_erlang_cast: no such connection %.*s\n",conname.len,conname.s);
		return -1;
	}
	LM_DBG("using listener %s\n",node->name);
	erl_cmd.cmd=ERLANG_CAST;
	erl_cmd.node=node;
	erl_cmd.reg_name=shm_strdup(&regproc);
	erl_cmd.ret_pv=NULL;
	erl_cmd.erlbuf=shm_malloc(AVP_PRINTBUF_SIZE);
	erlbuf.buff=erl_cmd.erlbuf;
	erlbuf.buffsz=AVP_PRINTBUF_SIZE-1;
	erlbuf.index=0;
	ei_x_encode_version(&erlbuf);
	//User passed term will be sedond element of tuple, first is our erlang pid 
	ei_x_encode_tuple_header(&erlbuf, 2);
	// we are hacking erlang pid so we can have worker system pid here
	memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
	erl_pid.num=getpid();
	ei_x_encode_pid(&erlbuf, &erl_pid);
	//so much for pid, now encode term
	if(ei_x_format_wo_ver(&erlbuf, printbuf)!=0) {
		LM_ERR("cmd_erlang_cast: cannot fromat erlang binary from query\n");
		return -1;
	}
	erl_cmd.erlbuf_len=erlbuf.index;
	bytessent=write(pipe_fds[1], &erl_cmd, sizeof(erl_cmd));
	LM_DBG("sent %d  %d %s %d %p %p\n",bytessent,
			erl_cmd.cmd,erl_cmd.reg_name,erl_cmd.erlbuf_len,erl_cmd.erlbuf, erl_cmd.node);
	return 1;
}



int send_erlang_cast(struct erlang_cmd *erl_cmd) {
    struct nodes_list *node;
    
    node=erl_cmd->node;
    if(ei_reg_send(&(node->ec), node->fd,
		erl_cmd->reg_name, erl_cmd->erlbuf, erl_cmd->erlbuf_len)<0) {
	LM_ERR("send_erlang_cast failed\n");
	return -1;
    }
    return 0;
}