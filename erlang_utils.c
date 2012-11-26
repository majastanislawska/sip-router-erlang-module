

#include "erlang_mod.h"

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
