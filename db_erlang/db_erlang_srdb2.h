#ifndef _ERLANG_SRDB2_H
#define _ERLANG_SRDB2_H  1

#include "../../lib/srdb2/db.h"
#include "../../lib/srdb2/db_drv.h"
#include "../../lib/srdb2/db_cmd.h"
#include <stdarg.h>

int erlang_srdb2_ctx(db_ctx_t* ctx);
int erlang_srdb2_con(db_con_t* con);
int erlang_srdb2_uri(db_uri_t* uri);
int erlang_srdb2_cmd(db_cmd_t* cmd);
int erlang_srdb2_cmd_exec(db_res_t* res, db_cmd_t* cmd);
int erlang_srdb2_cmd_first(db_res_t* res);
int erlang_srdb2_cmd_next(db_res_t* res);
int erlang_srdb2_res(db_cmd_t* cmd);
int erlang_srdb2_fld(db_fld_t* fld, char* table);
int erlang_srdb2_getopt(db_cmd_t* cmd, char* optname, va_list ap);
int erlang_srdb2_setopt(db_cmd_t* cmd, char* optname, va_list ap);

#endif
