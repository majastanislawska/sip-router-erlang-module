#ifndef _ERLANG_CMD_H
#define _ERLANG_CMD_H

#include "../../str.h"
#include "../../parser/msg_parser.h"

#include "erlang_listener.h"

#define ERLANG_CAST 1
#define ERLANG_CALL 2
#define ERLANG_REX  3
#define ERLANG_RPC  4

struct erlang_cmd {
    int cmd;
    struct nodes_list* node;
    int erlbuf_len;
    char *reg_name;
    pv_spec_t * ret_pv;
    char *erlbuf;
    int route_no;
    unsigned int tm_hash;
    unsigned int tm_label;
};

int fixup_cmd_erlang_cast(void** param, int param_no);
int cmd_erlang_cast(struct sip_msg* msg, char* str1, char* str2, char* str3);
int send_erlang_cast(struct erlang_cmd* erl_cmd);

int fixup_cmd_erlang_call(void** param, int param_no);
int cmd_erlang_call(struct sip_msg* msg, char *cn , char *dest, char *term, char *route, char *_ret_pv);
int send_erlang_call(struct erlang_cmd* erl_cmd);

int fixup_cmd_erlang_rex(void** param, int param_no);
int cmd_erlang_rex(struct sip_msg* msg, char *cn , char *mo, char *fu, char *ar, char *route, char *_ret_pv);
int send_erlang_rex(struct erlang_cmd* erl_cmd);

/* erlang_utils.c */
void utils_mk_ref(ei_cnode* ec, erlang_ref* ref);
/** @} */
#endif 
