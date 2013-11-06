#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "../../lvalue.h"

/* stolen from erts/emulator/beam/erl_term.h */
#define _REF_NUM_SIZE 18
#define MAX_REFERENCE (1 << _REF_NUM_SIZE)

/* generate reference struct for erlang_call  */
/* borowed from FreeSwitch */

void utils_mk_ref(ei_cnode* ec, erlang_ref* ref) {
    memset(ref, 0, sizeof(*ref));
    snprintf(ref->node, MAXATOMLEN, "%s", ec->thisnodename);
    lock_get(&(globals->ref_lock));
    globals->ref0++;
    if (globals->ref0 >= MAX_REFERENCE) {
	globals->ref0 = 0; globals->ref1++;
	if (globals->ref1 == 0) {
	    globals->ref2++;
	}
    }
    ref->n[0] = globals->ref0;
    ref->n[1] = globals->ref1;
    ref->n[2] = globals->ref2;
    lock_release(&(globals->ref_lock));
    ref->creation = 1;	/* why is this 1 */
    ref->len = 3;	/* why is this 3 */
    LM_DBG("new ref: %d.%d.%d@%s\n",ref->n[0],ref->n[1],ref->n[2],ref->node);
}
extern struct erlang_cmd *pending_cmds;

struct erlang_cmd *find_pending_by_pid(unsigned int num,unsigned int serial) {
    struct erlang_cmd **cmd_p, *current_cmd;
    current_cmd=NULL;
    cmd_p=&pending_cmds;
    while(*cmd_p) {
	if (((*cmd_p)->num == num) &&
	    ((*cmd_p)->serial == serial)) {
	    current_cmd=*cmd_p;
	    *cmd_p=(*cmd_p)->next;
	    break;
	}
	cmd_p=&((*cmd_p)->next);
    }
    return current_cmd;
}
struct erlang_cmd *find_pending_by_ref(unsigned int n0, unsigned int n1, unsigned int n2) {
    struct erlang_cmd **cmd_p, *current_cmd;
    current_cmd=NULL;
    cmd_p=&pending_cmds;
    while(*cmd_p) {
	if (((*cmd_p)->refn0 == n0) &&
	    ((*cmd_p)->refn1 == n1) &&
	    ((*cmd_p)->refn2 == n2)) {
	    current_cmd=*cmd_p;
	    *cmd_p=(*cmd_p)->next;
	    break;
	}
	cmd_p=&((*cmd_p)->next);
    }
    return current_cmd;
}

void fill_retpv(pv_spec_t *dst, ei_x_buff *buf ,int *decode_index) {
    if(dst==NULL)
	return;
    pv_value_t val;
    char *pbuf=NULL;
    pbuf=pkg_malloc(BUFSIZ);
    if (!pbuf) {
	LM_ERR("no shm memory\n\n");
	return;
    }
    ei_s_print_term(&pbuf, buf->buff, decode_index);
    LM_DBG("fill_retpv: %s\n", pbuf);
    val.rs.s = pbuf;
    val.rs.len = strlen(pbuf);
    val.flags = PV_VAL_STR;
    dst->setf(0, &dst->pvp, (int)EQ_T, &val);
    pkg_free(pbuf);
}
