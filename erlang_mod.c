#include "erlang_mod.h"
#include "erlang_cmd.h"
#include "erlang_listener.h"

#include "../../sr_module.h"
#include "../../cfg/cfg_struct.h"
#include "../../shm_init.h"
#include "../../dprint.h"

//global vars
struct globals_t *globals=0;
struct nodes_list* nodes_lst=0; //connected remote nodes 
//static struct ctrl_socket* ctrl_sock_lst=0;
static int fd_no=0; /* number of fd used */
//static int rpc_handler=0;

int pipe_fds[2] = {-1,-1};
/* tm */
struct tm_binds tm_api;

struct erlang_counters_h erlang_cnts_h;
counter_def_t erlang_cnt_defs[] =  {
	{&erlang_cnts_h.msg_sent, "messages sent", 0, 0, 0,
		"Counts messages sent to erlang VM."},
	{&erlang_cnts_h.msg_recv, "messages received", 0, 0, 0,
		"Counts messages received from erlang VM."},
	{0, 0, 0, 0, 0, 0 }
};

static int erlang_mod_init(void);
static void erlang_mod_destroy(void);
static int erlang_child_init(int rank);
static int add_node(modparam_t type, void * val);
static void free_node (struct nodes_list *node);

MODULE_VERSION

static void  ctrl_listen_ls_rpc(rpc_t* rpc, void* ctx) {return;};
void io_listen_who_rpc(rpc_t* rpc, void* ctx) {return;};
void io_listen_conn_rpc(rpc_t* rpc, void* ctx) {return;};


static char* io_listen_who_doc[]={ "list open connections", 0 };
static char* io_listen_conn_doc[]={ "returns number of open connections", 0 };
static char* ctl_listen_ls_doc[]={ "list ctl listen sockets", 0 };

static rpc_export_t erlang_rpc[]={
	{"erlang.nodes",    io_listen_who_rpc, (const char**)io_listen_who_doc, 0},
	{"erlang.alive",    io_listen_conn_rpc,(const char**)io_listen_conn_doc,0},
	{ 0, 0, 0, 0}
};

static cmd_export_t cmds[] = {
	{"erlang_cast", (cmd_function)cmd_erlang_cast, 3, fixup_cmd_erlang_cast, ANY_ROUTE},
	{"erlang_call", (cmd_function)cmd_erlang_call, 5, fixup_cmd_erlang_call, ANY_ROUTE},
	{"erlang_call_route", (cmd_function)cmd_erlang_call_route, 4, fixup_cmd_erlang_call_route, ANY_ROUTE},
	{"erlang_rex", (cmd_function)cmd_erlang_rex, 6, fixup_cmd_erlang_rex, ANY_ROUTE},
	{0, 0, 0, 0, 0}
};
/*
 * Exported parameters
 */
static param_export_t params[] = {
	{"connect",   PARAM_STRING|PARAM_USE_FUNC,	(void*) add_node},
	{0, 0, 0}
};

struct module_exports exports = {
	"erlang",
	cmds,
	erlang_rpc,      /* RPC method */
	params,          /*  module parameters */
	erlang_mod_init,  /* module initialization function */
	0,               /* response function */
	erlang_mod_destroy, /* destroy function */
	0,               /* oncancel function */
	erlang_child_init  /* per-child init function */
};

int mod_register(char *path, int *dlflags, void *p1, void *p2)
{
	erl_init(NULL,0);
	if(!shm_initialized() && init_shm()<0) {
		LM_ERR("cant init shm\n");
		return -1;
	}
//	return -1;
	return 0;
}
static int erlang_mod_init(void)
{

	LM_DBG("erlang_modinit, fd_no=%d\n", fd_no);
	if (load_tm_api(&tm_api)==-1) {
		LM_ERR("cannot load the TM-functions\n");
		return -1;
	}
	if (pipe(pipe_fds) < 0) {
		LM_ERR("erlang_mod_init: pipe() failed\n");
		return -1;
	}
	LM_DBG("erlang_mod_init, pipe_fds(%d, %d)\n", pipe_fds[0],pipe_fds[1]);
	globals=shm_malloc(sizeof(struct globals_t));
	if(globals==NULL) {
	    LM_ERR("erlang_mod_init: out of shm for globals\n");
	    return -1;
	}
	if (lock_init(&(globals->ref_lock))==0){
	    LM_ERR("erlang_mod_init: error initialising ref_lock\n");
	    return -1;
	  }
	  
	register_procs(1); /* we will be creating an extra process */
	register_fds(fd_no+2);
	cfg_register_child(1);

	return 0;
}
static int erlang_child_init(int rank)
{
	int pid;
	struct nodes_list* n;
	LM_DBG("erlang_child_init \n");
	if (rank==PROC_INIT) {
		LM_DBG("erlang_child_init: called for PROC_INIT dooing nothing\n");
	}
	if (rank == PROC_MAIN){
		LM_DBG("erlang_child_init: called for PROC_MAIN\n");
		for (n=nodes_lst; n; n=n->next){
			n->fd=-1;
			n->timeout=time(NULL);
//			node_connect_init(n);
		}
		pid=fork_process(PROC_RPC, "erlang nodes manager", 1);
		if (pid<0){
			return -1;
		}
		if (pid == 0){ /* child */
			DBG("erlang_child_init: child(%d)\n",rank);
			is_main=0;
			if (cfg_child_init()) return -1;
			DBG("erlang_child_init: after fork rank:%d caling child_loop\n",rank);
			close(pipe_fds[1]);
			child_loop(pipe_fds[0]);
		}else{ /* parent */
			DBG("erlang_child_init: parent(%d), child=%d\n",rank, pid);
		}
		return 0;
	}
	LM_DBG("erlang_child_init: called for other\n");

	return 0;
}

static void erlang_mod_destroy(void)
{
	if(globals!=NULL) shm_free(globals);
	free_node(nodes_lst);
	return;
}


static int add_node(modparam_t type, void * val)
{
	char *s;
	struct nodes_list* node;

	if ((type & PARAM_STRING)==0){
		LOG(L_CRIT, "erlang: add_node bad parameter type %d\n",type);
		return -1;
	}
	s=(char*)val;
	node=parse_connect_param(s, strlen(s)); 
	if (node==0){
		LOG(L_ERR, "ERROR: erlang: add_node: cant parse parameter \"%s\"\n", s);
		return -1;
	}
	node->next=nodes_lst;
	nodes_lst=node;
	fd_no++;
	return 0;
}

static void free_node (struct nodes_list *node) {
	if (node) {
		if(node->name) shm_free(node->name);
		if(node->cookie) shm_free(node->cookie);
		if(node->node) shm_free(node->node);
		free_node(node->next);
		shm_free(node);
	}
	return;
}
