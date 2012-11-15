#ifndef _ERLANG_MOD_H
#define _ERLANG_MOD_H

#include "../../counters.h"
#include "../../mem/shm_mem.h"
#include "../../modules/tm/tm_load.h"
#include <erl_interface.h>
#include <ei_connect.h>
#include <ei.h>

/* counter struct
*/
struct erlang_counters_h {
    counter_handle_t msg_sent;
    counter_handle_t msg_recv;
};
/* defined in km_dbase.c */
extern struct erlang_counters_h erlang_cnts_h;
extern struct tm_binds tm_api;
extern int pipe_fds[2];
extern struct tm_binds tm_api;
/** @} */
#endif /* _MYSQL_MOD_H */
