#ifndef _ERLANG_CMD_H
#define _ERLANG_CMD_H

#include "../../str.h"
#include "../../parser/msg_parser.h"
#include "../../locking.h"
#include "../../pvar.h"

#include "erlang_listener.h"

#define ERLANG_INFO       1
#define ERLANG_CAST       2
#define ERLANG_CALL       3
#define ERLANG_CALL_ROUTE 4
#define ERLANG_REX        5
#define ERLANG_RPC        6

struct erlang_cmd {
    gen_lock_t lock;
    int cmd;
    struct nodes_list* node;
    char *reg_name;
    pv_spec_t * ret_pv;
    char *erlbuf;
    int erlbuf_len;
    int decode_index;
    int route_no;
    int retcode;
    unsigned int num;
    unsigned int serial;
    unsigned int refn0;
    unsigned int refn1;
    unsigned int refn2;
    struct erlang_cmd *next;
};
extern struct erlang_cmd *pending_cmds;

int fixup_cmd_erlang_info(void** param, int param_no);
int cmd_erlang_info(struct sip_msg* msg, char* str1, char* str2, char* str3);

int fixup_cmd_erlang_cast(void** param, int param_no);
int cmd_erlang_cast(struct sip_msg* msg, char* str1, char* str2, char* str3);

int fixup_cmd_erlang_call(void** param, int param_no);
int cmd_erlang_call(struct sip_msg* msg, char *cn , char *dest, char *term, char *_ret_pv);

int fixup_cmd_erlang_call_route(void** param, int param_no);
int cmd_erlang_call_route(struct sip_msg* msg, char *cn , char *dest, char *term, char *_ret_pv);

int fixup_cmd_erlang_rex(void** param, int param_no);
int cmd_erlang_rex(struct sip_msg* msg, char *cn , char *mo, char *fu, char *ar, char *_ret_pv);

/* erlang_utils.c */
void utils_mk_ref(ei_cnode* ec, erlang_ref* ref);
struct erlang_cmd *find_pending_by_pid(unsigned int num,unsigned int serial);
struct erlang_cmd *find_pending_by_ref(unsigned int n0, unsigned int n1, unsigned int n2);
void fill_retpv(pv_spec_t *dst, ei_x_buff *buf ,int *decode_index);

int do_erlang_call(str *conname, str *regproc, ei_x_buff* payload, ei_x_buff* ret_buf);

/** @} */
#endif 
