#ifndef _DB_ERLANG_MOD_H
#define _DB_ERLANG_MOD_H

#include "../../counters.h"
#include <erl_interface.h>
#include <ei_connect.h>
#include <ei.h>
#include "../erlang/api.h"

/* counter struct
*/
struct erlang_counters_h {
    counter_handle_t msg_sent;
    counter_handle_t msg_recv;
};
/* defined in km_dbase.c */
extern struct erlang_counters_h erlang_cnts_h;

/** @} */
#endif /* _ERLANG_MOD_H */
