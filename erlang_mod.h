#ifndef _ERLANG_MOD_H
#define _ERLANG_MOD_H

#include "../../counters.h"
#include "../../mem/shm_mem.h"
#include <erl_interface.h>
#include <ei_connect.h>
#include <ei.h>

struct globals_t {
    gen_lock_t ref_lock;
    unsigned int ref0;
    unsigned int ref1;
    unsigned int ref2;
    int debug;
};
/* counter struct
*/
struct erlang_counters_h {
    counter_handle_t msg_sent;
    counter_handle_t msg_recv;
};
extern struct globals_t *globals;
extern struct erlang_counters_h erlang_cnts_h;
extern int pipe_fds[2];
extern int call_route_exit;
extern ei_x_buff lastterm;

/** @} */
#endif /* _ERLANG_MOD_H */
