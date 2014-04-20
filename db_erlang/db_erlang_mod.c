#include "db_erlang_mod.h"
#include "db_erlang_srdb1.h"
#include "db_erlang_srdb2.h"

#include "../../sr_module.h"
#include "../../lib/srdb1/db.h"
#include "../../lib/srdb2/db.h"
#include "../../dprint.h"

static int db_erlang_mod_init(void);
static int db_erlang_child_init(int rank);
int db_erlang_bind_db_api(db_func_t *dbb);
static int kam_db_erlang_mod_init(void);
static int kam_db_erlang_child_init(int rank);

MODULE_VERSION


/*
 * MySQL database module interface
 */
static cmd_export_t cmds[] = {
	{"db_ctx",      (cmd_function)NULL,                   0, 0, 0},
	{"db_ctx",      (cmd_function)erlang_srdb2_ctx,       0, 0, 0},
	{"db_con",      (cmd_function)erlang_srdb2_con,       0, 0, 0},
	{"db_uri",      (cmd_function)erlang_srdb2_uri,       0, 0, 0},
	{"db_cmd",      (cmd_function)erlang_srdb2_cmd,       0, 0, 0},
	{"db_put",      (cmd_function)erlang_srdb2_cmd_exec,  0, 0, 0},
	{"db_del",      (cmd_function)erlang_srdb2_cmd_exec,  0, 0, 0},
	{"db_get",      (cmd_function)erlang_srdb2_cmd_exec,  0, 0, 0},
	{"db_upd",      (cmd_function)erlang_srdb2_cmd_exec,  0, 0, 0},
	{"db_sql",      (cmd_function)erlang_srdb2_cmd_exec,  0, 0, 0},
	{"db_first",    (cmd_function)erlang_srdb2_cmd_first, 0, 0, 0},
	{"db_next",     (cmd_function)erlang_srdb2_cmd_next,  0, 0, 0},
	{"db_res",      (cmd_function)erlang_srdb2_res,       0, 0, 0},
	{"db_fld",      (cmd_function)erlang_srdb2_fld,       0, 0, 0},
	{"db_setopt",   (cmd_function)erlang_srdb2_setopt,    0, 0, 0},
	{"db_getopt",   (cmd_function)erlang_srdb2_getopt,    0, 0, 0},
	{"db_bind_api", (cmd_function)db_erlang_bind_db_api,  0, 0, 0},
	{0, 0, 0, 0, 0}
};
static kam_cmd_export_t kam_cmds[] = {
	{"db_bind_api",  (cmd_function)db_erlang_bind_db_api,      0, 0, 0, 0},
	{0, 0, 0, 0, 0, 0}
};

/*
 * Exported parameters
 */
static param_export_t params[] = {
//	{"ping_interval",   PARAM_INT, &my_ping_interval},
//	{"connect_timeout", PARAM_INT, &my_connect_to},
//	{"send_timeout",    PARAM_INT, &my_send_to},
//	{"receive_timeout", PARAM_INT, &my_recv_to},
//	{"retries",         PARAM_INT, &my_retries},
//	{"timeout_interval", INT_PARAM, &db_mysql_timeout_interval},
//	{"auto_reconnect",   INT_PARAM, &db_mysql_auto_reconnect},
	{0, 0, 0}
};
static param_export_t kam_params[] = {
//	{"ping_interval",    INT_PARAM, &db_mysql_ping_interval},
//	{"timeout_interval", INT_PARAM, &db_mysql_timeout_interval},
//	{"auto_reconnect",   INT_PARAM, &db_mysql_auto_reconnect},
	{0, 0, 0}
};


struct module_exports exports = {
	"db_erlang",
	cmds,
	0,               /* RPC method */
	params,          /*  module parameters */
	db_erlang_mod_init,  /* module initialization function */
	0,               /* response function*/
	0,               /* destroy function */
	0,               /* oncancel function */
	db_erlang_child_init  /* per-child init function */
};
struct kam_module_exports kam_exports = {
	"db_erlang",
	DEFAULT_DLFLAGS, /* dlopen flags */
	kam_cmds,
	kam_params,      /*  module parameters */
	0,               /* exported statistics */
	0,               /* exported MI functions */
	0,               /* exported pseudo-variables */
	0,               /* extra processes */
	kam_db_erlang_mod_init,  /* module initialization function */
	0,               /* response function*/
	0,               /* destroy function */
	kam_db_erlang_child_init   /* per-child init function */
};

struct erlang_binds erl_bind;

int mod_register(char *path, int *dlflags, void *p1, void *p2)
{
	erl_init(NULL,0);
//	return -1;
	return 0;
}
static int db_erlang_mod_init(void)
{
	erlang_load_api(&erl_bind);
	LM_DBG("db_erlang_mod_init - calling kam_relang_mod_init\n");
	return kam_db_erlang_mod_init();
}
int kam_db_erlang_mod_init(void)
{
	LM_DBG("kam_db_erlang_mod_init\n");
	return 0;
}
static int db_erlang_child_init(int rank)
{
//	LM_DBG("db_erlang_child_init \n");
	LM_DBG("erlang_modinit - calling kam_relang_mod_init\n");
	return kam_db_erlang_child_init(rank);
	return 0;
}
int kam_db_erlang_child_init(int rank)
{
	LM_DBG("kam_db_erlang_child_init\n");
	return 0;
}

int db_erlang_bind_db_api(db_func_t *dbb)
{
	LM_DBG("erlang_bind_db_api\n");
	if(dbb==NULL)
		return -1;
	memset(dbb, 0, sizeof(db_func_t));
	dbb->use_table        = erlang_srdb1_use_table;
	dbb->init             = erlang_srdb1_init;
	dbb->close            = erlang_srdb1_close;
	dbb->query            = erlang_srdb1_query;
	dbb->fetch_result     = NULL; //erlang_srdb1_fetch_result;
	dbb->raw_query        = NULL; //erlang_srdb1_raw_query;
	dbb->free_result      = erlang_srdb1_free_result;
	dbb->insert           = erlang_srdb1_insert;
	dbb->delete           = erlang_srdb1_delete;
	dbb->update           = erlang_srdb1_update;
	dbb->replace          = erlang_srdb1_replace;
	dbb->last_inserted_id = NULL; //erlang_srdb1_last_inserted_id;
	dbb->insert_update    = erlang_srdb1_insert_update;
	dbb->insert_delayed   = NULL; //erlang_srdb1_insert_delayed;
	dbb->affected_rows    = NULL; //erlang_srdb1_affected_rows;
	return 0;
}
