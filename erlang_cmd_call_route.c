#include <sys/types.h>
#include <unistd.h>
#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
#include "../../mod_fix.h"

int fixup_cmd_erlang_call_route(void** param, int param_no){
	if (param_no < 3)
		return fixup_spve_null(param, 1);
	if (param_no == 4) {
		return fixup_pvar_null(param, 1);
	}
	if (param_no == 3) {
		pv_elem_t *model=NULL;
		str s;
		s.s = (char*)(*param);
		s.len = strlen(s.s);
		if(s.len==0) {
			LM_ERR("cmd_erlang_call_route: param %d is empty string! please use erlang empty list [].\n", param_no);
			return -1;
		}
		if(pv_parse_format(&s ,&model) || model==NULL) {
			LM_ERR("cmd_erlang_call_route: wrong format [%s] for value param!\n", s.s);
			return -1;
		}
		*param = (void*)model;
		return 0;
	}
	LM_ERR("erlang_call_route takes exactly 4 parameters.\n");
	return -1;
}

int cmd_erlang_call_route(struct sip_msg* msg, char *cn , char *rp, char *ar, char *_ret_pv) {
#define AVP_PRINTBUF_SIZE 1024
	static char printbuf[AVP_PRINTBUF_SIZE];
	static char routename[MAXATOMLEN];
	int printbuf_len, bytessent, i, j;
	ei_x_buff argbuf,retbuf;
	struct nodes_list* node;
	pv_spec_t * ret_pv;
	str conname, regproc;
	struct run_act_ctx ra_ctx;
	int route_no;
	int retcode = -1;

	if(msg==NULL) {
		LM_ERR("cmd_erlang_call_route: received null msg\n");
		return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)cn, &conname)<0) {
		LM_ERR("cmd_erlang_call_route: cannot get the connection name\n");
		return -1;
	}
	for(node=nodes_lst;node;node=node->next) {
		if(strcmp(node->name, conname.s)==0) break;
	}
	if(node==0){
		LM_ERR("cmd_erlang_call_route: no such connection %.*s\n",conname.len,conname.s);
		return -1;
	}

	if(fixup_get_svalue(msg, (gparam_p)rp, &regproc)<0) {
		LM_ERR("cmd_erlang_call_route: cannot get the registered proc name\n");
		return -1;
	}

	printbuf_len = AVP_PRINTBUF_SIZE-1;
	if(pv_printf(msg, (pv_elem_p)ar, printbuf, &printbuf_len)<0 || printbuf_len<=0) {
		LM_ERR("erlang_cmd_call_route: cannot expand args expression.\n");
		return -1;
	}

	ret_pv = (pv_spec_t*)shm_malloc(sizeof(pv_spec_t));
	if (!ret_pv) {
		LM_ERR("no shm memory\n\n");
		return -1;
	}
	memcpy(ret_pv, (pv_spec_t *)_ret_pv, sizeof(pv_spec_t));

	LM_DBG("cmd_erlang_call_route:  %.*s %.*s %.*s\n",conname.len,conname.s,
			regproc.len,regproc.s, printbuf_len,printbuf);

	ei_x_new(&argbuf);
	if(ei_x_format_wo_ver(&argbuf, printbuf)!=0) {
		LM_ERR("cannot fromat erlang binary from arg string\n");
		goto error;
	}
	do_erlang_call(&conname, &regproc, &argbuf, &retbuf);
	//we have a two element tuple here
	ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &i);
	//first element should be atom we can add check here
	ei_get_type(retbuf.buff, &(retbuf.index), &i, &j);
	ei_decode_atom(retbuf.buff, &(retbuf.index), routename);
	//second element is put to ret_pv wihout bothering what is it
	fill_retpv(ret_pv,&retbuf,&(retbuf.index));
	if(retbuf.buff) shm_free(retbuf.buff);

	//execute route
	route_no=route_get(&main_rt, routename);
	if (route_no==-1){
		ERR("node_receive: failed to fix route \"%s\": route_get() failed\n",routename);
		goto error;
	}
	if (main_rt.rlist[route_no]==0){
		WARN("node_receive: route \"%s\" is empty / doesn't exist\n", routename);
	}
	init_run_actions_ctx(&ra_ctx);
	i=run_actions(&ra_ctx, main_rt.rlist[route_no], msg);
	if (i < 0) {
		LM_ERR("erlang_call_route: run_actions failed (%d)\n",i);
		goto error;
	}
	retcode=(call_route_exit)?0:1;
error:

	ei_x_free(&argbuf);
	return retcode;
}
