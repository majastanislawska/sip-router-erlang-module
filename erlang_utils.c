#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"
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
    ei_s_print_term(&pbuf, buf->buff, decode_index);
    LM_DBG("fill_retpv: %s\n", pbuf);
    val.rs.s = pbuf;
    val.rs.len = strlen(pbuf);
    val.flags = PV_VAL_STR;
    dst->setf(0, &dst->pvp, (int)EQ_T, &val);
    free(pbuf);
}

int do_erlang_call(str *conname, str *regproc, ei_x_buff* payload, ei_x_buff *ret_buf) {
#define AVP_PRINTBUF_SIZE 1024
    struct nodes_list* node;
    struct erlang_cmd *erl_cmd;
    ei_x_buff argbuf;
    erlang_pid erl_pid;
    erlang_ref ref;
    int bytessent,retcode;
#ifdef DEBUG
    int i=1;
    char *pbuf= NULL;
#endif

    for(node=nodes_lst;node;node=node->next) {
	LM_DBG("do_erlang_call: matching %s with %.*s\n",node->name,conname->len,conname->s);
	if(strcmp(node->name, conname->s)==0) break;
    }
    if(node==0){
	LM_ERR("do_erlang_call: no such connection %.*s\n",conname->len,conname->s);
	return -1;
    }
    erl_cmd=shm_malloc(sizeof(struct erlang_cmd));
    if(!erl_cmd) {
	LM_ERR("no shm");
	return -1;
    }
    memset(erl_cmd,0,sizeof(struct erlang_cmd));

    if(lock_init(&(erl_cmd->lock))==NULL) {
	LM_ERR("cannot init the lock\n");
	goto error;
    }
    erl_cmd->reg_name=shm_strdup(regproc);
    if (!erl_cmd->reg_name) {
	LM_ERR("no shm memory\n\n");
	goto error;
    }
    LM_DBG("do_erlang_call:  %.*s %.*s\n",conname->len,conname->s,
		regproc->len,regproc->s);
    erl_cmd->cmd=ERLANG_CALL;
    erl_cmd->node=node;
    ei_x_new(&argbuf);
    ei_x_encode_version(&argbuf);
    // gen_call is 3-element tuple, first is atom $gen_call,.
    // second is {pid,ref} tuple and third is user data.
    ei_x_encode_tuple_header(&argbuf, 3);
    ei_x_encode_atom(&argbuf, "$gen_call");
    ei_x_encode_tuple_header(&argbuf, 2);
    // we are hacking erlang pid so we can have worker system pid here
    memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
    ei_x_encode_pid(&argbuf, &erl_pid);
    utils_mk_ref(&(node->ec),&ref);
    ei_x_encode_ref(&argbuf, &ref);
    erl_cmd->refn0=ref.n[0];
    erl_cmd->refn1=ref.n[1];
    erl_cmd->refn2=ref.n[2];

    ei_x_append(&argbuf, payload);
    erl_cmd->erlbuf_len=argbuf.index;
    erl_cmd->erlbuf=shm_malloc(argbuf.index);
    if (!erl_cmd->erlbuf) {
	LM_ERR("no shm memory\n\n");
	goto error;
    }
    memcpy(erl_cmd->erlbuf,argbuf.buff,argbuf.index);
#ifdef DEBUG
    ei_s_print_term(&pbuf, erl_cmd->erlbuf, &i);
    LM_DBG("message is pbuf='%s' buf.buffsz=%d buf.index=%d i=%d\n", pbuf, argbuf.buffsz,argbuf.index,i );
    free(pbuf);pbuf=NULL;
#endif
    lock_get(&(erl_cmd->lock));
    bytessent=write(pipe_fds[1], &erl_cmd, sizeof(erl_cmd));
    LM_DBG("do_erlang_call: locked, sent %d  %d %s %d %p %p, waiting for release\n",bytessent,
			erl_cmd->cmd,erl_cmd->reg_name,erl_cmd->erlbuf_len,erl_cmd->erlbuf, erl_cmd);
    lock_get(&(erl_cmd->lock));
    LM_DBG("after lock\n");
    if (erl_cmd->retcode <0) {
	retcode=erl_cmd->retcode;
	LM_DBG("cmd_erlang_call: failed %d\n",retcode);
	goto error;
    }
    if (ret_buf) {
        ret_buf->buff=erl_cmd->erlbuf;
        ret_buf->buffsz=erl_cmd->erlbuf_len;
        ret_buf->index=erl_cmd->decode_index;
    } else {
	shm_free(erl_cmd->erlbuf); //caller ignores response
    }
    shm_free(erl_cmd->reg_name);
    shm_free(erl_cmd);
    return 1;

error:
    if(erl_cmd) {
	if(erl_cmd->ret_pv) shm_free(erl_cmd->ret_pv);
	if(erl_cmd->erlbuf) shm_free(erl_cmd->erlbuf);
	if(erl_cmd->reg_name) shm_free(erl_cmd->reg_name);
	shm_free(erl_cmd);
    }
    ei_x_free(&argbuf);
    return retcode;

}
