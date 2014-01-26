#include <sys/types.h>
#include <unistd.h>
#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
#include "../../mod_fix.h"

int fixup_cmd_erlang_call(void** param, int param_no){
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
			LM_ERR("cmd_erlang_call: param %d is empty string! please use erlang empty list [].\n", param_no);
			return -1;
		}
		if(pv_parse_format(&s ,&model) || model==NULL) {
			LM_ERR("cmd_erlang_call: wrong format [%s] for value param!\n", s.s);
			return -1;
		}
		*param = (void*)model;
		return 0;
	}
	LM_ERR("erlang_call takes exactly 4 parameters.\n");
	return -1;
}

int cmd_erlang_call(struct sip_msg* msg, char *cn, char *rp, char *ar, char *_ret_pv) {
#define AVP_PRINTBUF_SIZE 1024
	static char printbuf[AVP_PRINTBUF_SIZE];
	int printbuf_len;
	ei_x_buff argbuf,retbuf;
	struct nodes_list* node;
	pv_spec_t * ret_pv;
	str conname, regproc;
	int retcode = -1;

	if(msg==NULL) {
		LM_ERR("cmd_erlang_call: received null msg\n");
		return -1;
	}
	if(fixup_get_svalue(msg, (gparam_p)cn, &conname)<0) {
		LM_ERR("cmd_erlang_call: cannot get the connection name\n");
		return -1;
	}
	for(node=nodes_lst;node;node=node->next) {
		if(strcmp(node->name, conname.s)==0) break;
	}
	if(node==0) {
		LM_ERR("cmd_erlang_call: no such connection %.*s\n",conname.len,conname.s);
		return -1;
	}

	if(fixup_get_svalue(msg, (gparam_p)rp, &regproc)<0) {
		LM_ERR("cmd_erlang_call: cannot get the registered proc name\n");
		return -1;
	}
	printbuf_len = AVP_PRINTBUF_SIZE-1;
	if(pv_printf(msg, (pv_elem_p)ar, printbuf, &printbuf_len)<0 || printbuf_len<=0) {
		LM_ERR("erlang_cmd_call: cannot expand args expression.\n");
		return -1;
	}

	ret_pv = (pv_spec_t*)shm_malloc(sizeof(pv_spec_t));
	if (!ret_pv) {
		LM_ERR("no shm memory\n\n");
		return -1;
	}
	memcpy(ret_pv, (pv_spec_t *)_ret_pv, sizeof(pv_spec_t));

	LM_DBG("cmd_erlang_call:  %.*s %.*s %.*s\n",conname.len,conname.s,
			regproc.len,regproc.s, printbuf_len,printbuf);

	ei_x_new(&argbuf);
	if(ei_x_format_wo_ver(&argbuf, printbuf)!=0) {
		LM_ERR("cannot fromat erlang binary from arg string\n");
		goto error;
	}
	do_erlang_call(&conname, &regproc, &argbuf, &retbuf);
	fill_retpv(ret_pv,&retbuf,&(retbuf.index));
	if(retbuf.buff) shm_free(retbuf.buff);
	retcode=1;
error:
	ei_x_free(&argbuf);
	return retcode;
}
