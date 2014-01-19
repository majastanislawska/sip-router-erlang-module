#include "erlang_mod.h"
#include "erlang_listener.h"
#include "erlang_cmd.h"
#include "../../sr_module.h"
#include "../../dprint.h"
#include "../../shm_init.h"
#include "../../cfg/cfg_struct.h"

struct erlang_cmd *pending_cmds = 0;
//forward decl
int send_erlang(struct erlang_cmd *erl_cmd);

void child_loop(int data_pipe)
{
    fd_set fdset;
    int selret,maxfd;
    struct nodes_list *node;
    struct erlang_cmd *erl_cmd;
    struct timeval tv;
    int readcount;
    int erl_retcode=-1;

    while(1) {
	FD_ZERO(&fdset);
	FD_SET(data_pipe, &fdset);
//	LM_DBG("child_loop: datapipe FD_SET(%d)\n",data_pipe);
	maxfd=data_pipe;
	tv.tv_sec=10; tv.tv_usec=0;
	
//	LM_DBG("erlang_child_loop: loopstart\n");
	for(node=nodes_lst; node; node=node->next){
//	    LM_DBG("child_loop: checking %s fd=%d\n",node->node,node->fd);
	    if(node_reconnect(node)<0) {
//		LM_DBG("child_loop: skipping %s\n",node->node);
		continue;
	    }
//	    LM_DBG("child_loop: Setting FD_SET(%d) %s\n",node->fd,node->node);
	    FD_SET(node->fd, &fdset);
	    maxfd=(node->fd > maxfd) ? node->fd : maxfd;
	}
	selret = select(maxfd+1, &fdset, NULL, NULL, &tv);
//	LM_DBG("erlang_child_loop: after select\n");
	cfg_update();

	if(selret < 0) {
	    LM_ERR("erlang_child_loop: error in select(): %d %s\n",errno,strerror(errno));
	    continue;
	}
	if(selret==0) {
//	    LM_DBG("erlang_child_loop: timeout\n");
	    continue;
	}
	for(node=nodes_lst; node; node=node->next){
	    if(node->fd<=0) //skip disconected nodes
	        continue;		
	    if(FD_ISSET(node->fd, &fdset)) {
		node_receive(node);
	    }
	}

	if(FD_ISSET(data_pipe, &fdset)) {
	    int unlock=0;
	    readcount=read(data_pipe, &erl_cmd, sizeof(erl_cmd));
	    LM_DBG("erlang_child_loop: read from worker %d %d %s %d %p %p\n",readcount,
			erl_cmd->cmd,erl_cmd->reg_name,erl_cmd->erlbuf_len,erl_cmd->erlbuf,erl_cmd->node);
	    node=erl_cmd->node;
	    switch(erl_cmd->cmd) {
		case ERLANG_INFO:
		case ERLANG_CAST: 
		    erl_retcode=send_erlang(erl_cmd);
		    unlock=1;
		    break;
		case ERLANG_CALL:
		case ERLANG_CALL_ROUTE:
		case ERLANG_REX:
		    erl_retcode=send_erlang(erl_cmd);
		    break;
		default:
		    LM_ERR("erlang_child_loop: unknown cmd_pipe command: %d\n",erl_cmd->cmd);
	    }
	    //not needed anymore, will be malloced again when response arrives
	    shm_free(erl_cmd->erlbuf);
	    erl_cmd->erlbuf=NULL;
	    erl_cmd->erlbuf_len=0;

	    if(erl_retcode<0) {
		LM_ERR("erlang_child_loop: erl_send failed: %d (unlocking)\n",erl_retcode);
		erl_cmd->retcode=erl_retcode;
		lock_release(&(erl_cmd->lock)); //unlock and let it fail in worker
		continue;
	    }
	    LM_DBG("erlang_child_loop: erl_send success: %d\n",erl_retcode);
	    if(unlock) {
		lock_release(&(erl_cmd->lock));
	    } else {
		//no unlock, hold worker until reply comes
		erl_cmd->next=pending_cmds;
		pending_cmds=erl_cmd;
	    }
	    continue;
	}
    }
}

int node_reconnect(struct nodes_list *node)
{
    time_t t;
    if(node->fd >0) return 0;
    t=time(NULL);
    if(node->timeout > t) {
//	LM_DBG("reconnect not yet %d sec left\n",(int)(node->timeout) - (int)t);
	return -1;
    }
//    LM_DBG("erlang conncting to %s %s %s\n",node->name, node->cookie,node->node);
    if (ei_connect_init(&(node->ec), node->name, node->cookie, 1) < 0) {
	LM_ERR("node_reconnect_loop: ei_connect_init error %s sheulding for sleep\n",node->name);
	node->timeout=t+30;
	node->fd=-1;
	return -1;
    }
//    LM_DBG("erlang_child_init: ei_connect_init sucseded %s\n",node->node);
    node->fd = ei_connect(&(node->ec), node->node);
    if(node->fd <0) {
	LM_ERR("erlang_new_connection ei_connect error %s %d sleeping for a while\n", node->name, node->fd);
	node->timeout=t+30;
	return -1;
    }
    LM_INFO("erlang_child_init: ei_connect connected to  %s fd=%d\n",node->node, node->fd);
    return 0;
}
void node_receive(struct nodes_list *node)
{
    int status = 1;
    char *pbuf= NULL;
    char name[MAXATOMLEN];
    int i=0,j=0, decode_index=0;
    struct erlang_cmd *current_cmd;
    erlang_ref ref;
    erlang_msg msg;
    ei_x_buff buf;
    
    ei_x_new(&buf);

//    LM_DBG("node_receive %s  fd=%d \n",node->node, node->fd);
    memset(&msg,0,sizeof(msg));
    status = ei_xreceive_msg_tmo(node->fd, &msg, &buf, 1000);
//    LM_DBG("node_receive %s fd=%d status=%d\n",node->node, node->fd, status);
    switch (status) {
	case ERL_TICK:
//	    LM_DBG("node_receive ERL_TICK\n");
	    break;
	case ERL_MSG:
	    LM_DBG("node_receive ERL_MSG\n");
	    switch (msg.msgtype) {
		case ERL_SEND:
		    LM_DBG("node_receive ERL_MSG erl_send from %s:<%d.%d.%d> to %s,<%d.%d.%d> to %s (cookie %s)\n", 
			    msg.from.node, msg.from.num, msg.from.serial, msg.from.creation,
			    msg.to.node,msg.to.num,msg.to.serial,msg.to.creation,
			    msg.toname,msg.cookie);
		    decode_index=0;
		    ei_decode_version(buf.buff,&decode_index,&j);
//debug 
		    i=decode_index;
		    pbuf=pkg_malloc(BUFSIZ);
		    LM_DBG("node_receive: buf.index=%d decode_index=%d i=%d j=%d\n", buf.index, decode_index,i,j );
		    ei_s_print_term(&pbuf, buf.buff, &i);
		    LM_DBG("node_receive: message is pbuf='%s' buf.buffsz=%d buf.index=%d decode_index=%d i=%d j=%d\n", pbuf, buf.buffsz,buf.index, decode_index,i,j );
		    pkg_free(pbuf);
//end debug
		    ei_get_type(buf.buff, &decode_index, &i, &j); //i is type, j is size
		    LM_DBG("node_receive: buf.index=%d decode_index=%d i=%d j=%d\n", buf.index, decode_index,i,j );
		    if((i==ERL_SMALL_TUPLE_EXT || i==ERL_LARGE_TUPLE_EXT) && j==2) {
			ei_decode_tuple_header(buf.buff, &decode_index, &i); // ignoring i value we know it is 2
			ei_get_type(buf.buff, &decode_index, &i, &j); //i is type, j is size
			switch(i) {
			    case ERL_ATOM_EXT:
				ei_decode_atom(buf.buff, &decode_index, name);
				if(strcmp("rex",name)==0) {
				    LM_DBG("got rex!\n");
				    current_cmd=find_pending_by_pid(msg.to.num,msg.to.serial);
				    if(current_cmd!=NULL) {
					LM_DBG("node_receive: got rex response unlcoking  %p\n",current_cmd);
					current_cmd->retcode=0;
					current_cmd->erlbuf=shm_malloc(buf.buffsz);
					memcpy(current_cmd->erlbuf, buf.buff, buf.buffsz);
					current_cmd->erlbuf_len=buf.buffsz;
					current_cmd->decode_index=decode_index;
					lock_release(&(current_cmd->lock));
				    } else {
					i=decode_index;
					pbuf=pkg_malloc(BUFSIZ);
					LM_DBG("node_receive: buf.index=%d decode_index=%d i=%d j=%d\n", buf.index, decode_index,i,j );
					ei_s_print_term(&pbuf, buf.buff, &i);
					LM_ERR("node_receive: Unexpected message  pbuf='%s' buf.index=%d decode_index=%d i=%d j=%d\n", pbuf, buf.index, decode_index,i,j );
					pkg_free(pbuf);
				    }
				} else {
				    LM_ERR("Don't know how to handle msg with {%s,...}\n",name);
				}
				break;
//			    case ERL_PID_EXT:
//				ei_decode_pid(buf.buff, &decode_index, &pid);
//				
//				break;
			    case ERL_NEW_REFERENCE_EXT:
			    case ERL_REFERENCE_EXT:
				    ei_decode_ref(buf.buff, &decode_index, &ref);
				    current_cmd=find_pending_by_ref(ref.n[0], ref.n[1], ref.n[2]);
				    if(current_cmd!=NULL) {
					LM_DBG("node_receive: got call or call_route response unlcoking  %p\n",current_cmd);
					current_cmd->retcode=0;
					current_cmd->erlbuf=shm_malloc(buf.buffsz);
					memcpy(current_cmd->erlbuf, buf.buff, buf.buffsz);
					current_cmd->erlbuf_len=buf.buffsz;
					current_cmd->decode_index=decode_index;
					lock_release(&(current_cmd->lock));
				    }else {
					i=decode_index;
					pbuf=pkg_malloc(BUFSIZ);
					LM_DBG("node_receive: buf.index=%d decode_index=%d i=%d j=%d\n", buf.index, decode_index,i,j );
					ei_s_print_term(&pbuf, buf.buff, &i);
					LM_ERR("node_receive: Unexpected message  pbuf='%s' buf.index=%d decode_index=%d i=%d j=%d\n", pbuf, buf.index, decode_index,i,j );
					pkg_free(pbuf);
				    }
				break;
			    default:
				LM_ERR("node_receive: expected atom or pid as 1st element of tuple (GOT %c)\n",i);
			}
			LM_DBG("end of node_receive case ERL_SEND!\n");
		    } else {
			LM_DBG("node_receive: not ERL_SMALL_TUPLE nor ERL_LARGE_TUPLE_EXT i=%d j=%d\n", i, j );
		    }
		    break;
		case ERL_REG_SEND:
		    LM_DBG("erl_reg_send to %s from %s:<%d.%d.%d> to %s:<%d.%d.%d>\n", msg.toname,
							msg.from.node,msg.from.num,msg.from.serial,msg.from.creation,
							msg.to.node,msg.to.num,msg.to.serial,msg.to.creation);
		    decode_index=0;
		    ei_decode_version(buf.buff,&decode_index,&j);
		    i=decode_index;
//debug
		    pbuf=pkg_malloc(BUFSIZ);
		    ei_s_print_term(&pbuf, buf.buff, &i);
		    LM_DBG("erl_send: message is '%s' %d %d %d\n", pbuf, buf.index, i,j );
		    pkg_free(pbuf);
//end debug
		    break;
		case ERL_LINK:
		    LM_DBG("erl_link\n");
		    break;
		case ERL_UNLINK:
		    LM_DBG("erl_unlink\n");
		    break;
		case ERL_EXIT:
		    LM_DBG("erl_exit from %s <%d.%d.%d>\n", msg.from.node, msg.from.creation, msg.from.num,msg.from.serial);
//			handle_exit(node, &msg.from);
		    break;
		default:
		    LM_DBG("unexpected msg type %d\n", (int) (msg.msgtype));
		    break;
		}
		break;
	case ERL_ERROR:
	    LM_DBG("node_receive ERL_ERROR\n");
		if (erl_errno != ETIMEDOUT && erl_errno != EAGAIN) {
		    LM_DBG("erl_error\n");
		    if(close(node->fd)) {
			LM_DBG("Error while closing fd after erl_error\n");
		    }
		    node->fd=-1;
		}
		break;
	default:
	    LM_DBG("node_receive unexpected status %d \n", status);
	    break;
    }
    ei_x_free(&buf);
}

char *shm_strdup(str *src)
{
	char *res;

	if (!src || !src->s)
		return NULL;
	if (!(res = (char *) shm_malloc(src->len + 1)))
		return NULL;
	strncpy(res, src->s, src->len);
	res[src->len] = 0;
	return res;
}

struct nodes_list *parse_connect_param(char *s, int l)
{
	struct nodes_list *node;
	str st;
	param_t* params_list = NULL;
	param_hooks_t phooks;
	param_t *pit=NULL;
	
	st.s = s;
	st.len = l;
	if(st.s[st.len-1]==';')
		st.len--;
	if (parse_params(&st, CLASS_ANY, &phooks, &(params_list))<0) {
		LM_ERR("ERROR:parse_connect_param: cant parse parameters\n");
		return 0;
	}
	node=shm_malloc(sizeof(struct nodes_list));
	memset(node,0,sizeof(struct nodes_list));
	if (node==0){
		LM_ERR("ERROR:parse_connect_param: out of memory\n");
		return 0;
	}
	for (pit = params_list; pit; pit=pit->next) {
		if (pit->name.len==4 && strncasecmp(pit->name.s, "name", 4)==0) {
			node->name = shm_strdup(&(pit->body));
		} else if(pit->name.len==6 && strncasecmp(pit->name.s, "cookie", 6)==0) {
			node->cookie= shm_strdup(&(pit->body));
		}  else if(pit->name.len==4 && strncasecmp(pit->name.s, "node", 4)==0) {
			node->node = shm_strdup(&(pit->body));
		}
	}
	if(node->name==NULL) {   LM_ERR("invalid node name\n");goto error;}
	if(node->cookie==NULL) { LM_ERR("cookie is not set\n");goto error;}
	if(node->node==NULL) {   LM_ERR("node is not set\n");  goto error;}
	
	free_params(params_list);
	LM_DBG("parsed: name=%s cookie=%s node=%s\n",node->name,node->cookie,node->node);
	return node;
error:
	if (node) {
	    if(node->name) shm_free(node->name);
	    if(node->cookie) shm_free(node->cookie);
	    if(node->node) shm_free(node->node);
	    shm_free(node);
	}
	free_params(params_list);
	return 0;
}

int send_erlang(struct erlang_cmd *erl_cmd) {
    struct nodes_list *node;

    node=erl_cmd->node;
    return ei_reg_send(&(node->ec), node->fd, erl_cmd->reg_name,
	erl_cmd->erlbuf, erl_cmd->erlbuf_len);
}
