#ifndef _DB_ERLANG_MOD_H
#define _DB_ERLANG_MOD_H

#include "../../counters.h"
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

struct erl_con {
	struct db_id* id;        /*!< Connection identifier */
	unsigned int ref;        /*!< Reference count */
	struct pool_con* next;   /*!< Next connection in the pool */

	int fd;
	ei_cnode ec;
//<------>MYSQL_RES* res;          /*!< Actual result */
//<------>MYSQL* con;              /*!< Connection representation */
//<------>MYSQL_ROW row;           /*!< Actual row in the result */
//<------>time_t timestamp;        /*!< Timestamp of last query */
};
#define CON_FD(db_con)     (((struct erl_con*)((db_con)->tail))->fd)
#define CON_EC(db_con)     (((struct erl_con*)((db_con)->tail))->ec)
//#define CON_ROW(db_con)        (((struct my_con*)((db_con)->tail))->row)
//#define CON_TIMESTAMP(db_con)  (((struct my_con*)((db_con)->tail))->timestamp)

/** @} */
#endif /* _MYSQL_MOD_H */
