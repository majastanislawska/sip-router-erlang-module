#ifndef _ERLANG_SELECT_H
#define _ERLANG_SELECT_H

#include "../../select.h"
#include "../../select_buf.h"

int sel_erl(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_scan(str* res, select_t* s, struct sip_msg* msg);
int sel_erl_get_type(str* res, select_t* s, struct sip_msg* msg);
int sel_erl_print(str* res, select_t* s, struct sip_msg* msg);
int sel_erl_value(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_size(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_int(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_string(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_float(str* res, select_t* s, struct sip_msg* msg);
#endif