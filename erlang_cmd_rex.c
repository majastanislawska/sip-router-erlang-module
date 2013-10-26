#include <sys/types.h>
#include <unistd.h>
#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
#include "../../mod_fix.h"

struct tm_binds tm_api;

int fixup_cmd_erlang_rex(void** param, int param_no){
	if (param_no <= 3 ||param_no == 5)
		return fixup_spve_null(param, 1);
	if (param_no == 6) {
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
	LM_ERR("erlang_rex takes exactly 6 parameters.\n");
	return -1;
}

int cmd_erlang_rex(struct sip_msg* msg, char *cn , char *mo, char *fu, char *ar, char *rt, char *_ret_pv) {
#define AVP_PRINTBUF_SIZE 1024
	static char printbuf[AVP_PRINTBUF_SIZE];
	int printbuf_len, bytessent, status;
	ei_x_buff argbuf;
	struct nodes_list* node;
	struct erlang_cmd erl_cmd;
	erlang_pid erl_pid;
	str conname, mod, fun, args, ret, route;
	pv_spec_t *ret_pv;
	
	memset(&erl_cmd,0,sizeof(struct erlang_cmd));
	if(msg==NULL) {
	    LM_ERR("received null msg\n");
	    return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)cn, &conname)<0) {
	    LM_ERR("cannot get the connection name\n");
	    return -1;
	}
	for(node=nodes_lst;node;node=node->next) {
		LM_DBG("matching %s with %.*s\n",node->name,conname.len,conname.s);
		if(strcmp(node->name, conname.s)==0) break;
	}
	if(node==0){ 
		LM_ERR("no such connection %.*s\n",conname.len,conname.s);
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
	if(fixup_get_svalue(msg, (gparam_p)rt, &route)<0) {
	    LM_ERR("cmd_erlang_rex: cannot get the ok_route name\n");
	    return -1;
	}
	erl_cmd.route_no=route_get(&main_rt, route.s);
	if (erl_cmd.route_no==-1){
		ERR("cmd_erlang_rex: failed to fix route \"%.*s\": route_get() failed\n",route.len,route.s);
		return -1;
	}
	if (main_rt.rlist[erl_cmd.route_no]==0){
		WARN("erlang_cmd_rex: route \"%.*s\" is empty / doesn't exist\n", route.len,route.s);
	}
	
//	if(fixup_get_svalue(msg, rt, &ret)<0) {
//	    LM_ERR("cannot get the ret pv_spec\n");
//	    return -1;
//	}
	printbuf_len = AVP_PRINTBUF_SIZE-1;
	if(pv_printf(msg, (pv_elem_p)ar, printbuf, &printbuf_len)<0 || printbuf_len<=0)
	{
		LM_ERR("erlang_cmd_rex: cannot expand args expression.\n");
		return -1;
	}

	tm_cell_t *t = 0;
	t = tm_api.t_gett();
	if (t==NULL || t==T_UNDEFINED) {
	    if(tm_api.t_newtran(msg)<0) {
		LM_ERR("cmd_erlang_rex: cannot create the transaction\n");
		return -1;
	    }
	    t = tm_api.t_gett();
	    if (t==NULL || t==T_UNDEFINED) {
		LM_ERR("cmd_erlang_rex: cannot look up the transaction\n");
		return -1;
	    }
	}

	ret_pv = (pv_spec_t*)shm_malloc(sizeof(pv_spec_t));
	if (!ret_pv) {
		LM_ERR("no shm memory\n\n");
		return -1;
	}
	memcpy(ret_pv, (pv_spec_t *)_ret_pv, sizeof(pv_spec_t));

	LM_DBG("cmd_erlang_rex:  %.*s %.*s %.*s %.*s\n",conname.len,conname.s,
			mod.len,mod.s, fun.len,fun.s, printbuf_len,printbuf);

	if (tm_api.t_suspend(msg, &(erl_cmd.tm_hash), &(erl_cmd.tm_label)) < 0) {
	    LM_ERR("cmd_erlang_rex: t_suspend() failed\n");
	    shm_free(ret_pv);
	    return -1;
	}
	LM_DBG("cmd_erlang_rex: request suspended hash=%d label=%d\n",erl_cmd.tm_hash,erl_cmd.tm_label);
	erl_cmd.cmd=ERLANG_REX;
	erl_cmd.node=node;
	erl_cmd.reg_name=0; /* pass null as we know recepitient reg_name on the other side*/
	erl_cmd.ret_pv=ret_pv;
	erl_cmd.erlbuf=shm_malloc(AVP_PRINTBUF_SIZE);
	argbuf.buff=erl_cmd.erlbuf;
	argbuf.buffsz=AVP_PRINTBUF_SIZE-1;
	argbuf.index=0;
//	ei_x_encode_empty_list(&argbuf);
	ei_x_encode_version(&argbuf);
	//User passed term will be second element of tuple, first is our (hacked) erlang pid 
	ei_x_encode_tuple_header(&argbuf, 2);
	// we are hacking erlang pid so we can have worker system pid here
	memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
	erl_pid.num=erl_cmd.tm_hash;  /* put tm_suspend params as pid so listener can easily get those back in reply*/
	erl_pid.serial=erl_cmd.tm_label;
	ei_x_encode_pid(&argbuf, &erl_pid);
	//so much for pid, now encode rex call tuple {call, mod, fun, arg, user}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"call");
	ei_x_encode_atom_len(&argbuf, mod.s, mod.len);
	ei_x_encode_atom_len(&argbuf, fun.s, fun.len);
	if(ei_x_format_wo_ver(&argbuf, printbuf)!=0) {
		LM_ERR("cannot fromat erlang binary from arg string\n");
		return -1;
	}
	ei_x_encode_atom(&argbuf,"user");
	erl_cmd.erlbuf_len=argbuf.index;
	bytessent=write(pipe_fds[1], &erl_cmd, sizeof(erl_cmd));
	LM_DBG("cmd_erlang_rex: exiting, sent %d  %d %s %d %p %p\n",bytessent,
			erl_cmd.cmd,erl_cmd.reg_name,erl_cmd.erlbuf_len,erl_cmd.erlbuf, erl_cmd.node);
	return 0;
}


int send_erlang_rex(struct erlang_cmd *erl_cmd) {
    struct nodes_list *node;
    struct pending_cmd *cmd;

    node=erl_cmd->node;
    if(ei_reg_send(&(node->ec), node->fd, "rex",
		erl_cmd->erlbuf, erl_cmd->erlbuf_len)<0) {
	LM_ERR("send_erlang_rex failed\n");
    return -1;
    }
    cmd=shm_malloc(sizeof(struct pending_cmd));
    if (cmd==0) {
	LM_ERR("no shm memory\n");
	return -2;
    }
    cmd->ret_pv=erl_cmd->ret_pv;
    cmd->route_no=erl_cmd->route_no;
    cmd->num=erl_cmd->tm_hash & 0x7FFF;    //mask bits that got trough erlang pid
    cmd->serial=erl_cmd->tm_label & 0x1FFF;// --,,--
    cmd->tm_hash=erl_cmd->tm_hash;
    cmd->tm_label=erl_cmd->tm_label;
    cmd->next=pending_cmds;
    pending_cmds=cmd;
    return 0;
}
