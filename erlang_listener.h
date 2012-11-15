#ifndef _ERLANG_LISTENER_H
#define _ERLANG_LISTENER_H

#include <time.h>

#include "../../parser/parse_param.h"
struct nodes_list{
	char *name;
	char *cookie;
	char *node;
//	int id;
	ei_cnode ec;
	int fd; /*file desctiptor for communication with erlang*/
	time_t timeout;
	struct nodes_list* next;
};
extern struct nodes_list* nodes_lst;

struct pending_cmd {
	pv_spec_t * ret_pv;
	int route_no;
	unsigned int num;
	unsigned int serial;
	unsigned int tm_hash;
	unsigned int tm_label;
	struct pending_cmd *next;
};
extern struct pending_cmd *pending_cmds;

void child_loop(int data_pipe);
int node_reconnect(struct nodes_list *node);
void node_receive(struct nodes_list *node);

struct nodes_list *parse_connect_param(char *s, int l);

char *shm_strdup(str *src);

#endif