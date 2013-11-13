#include "db_erlang_mod.h"
#include "db_erlang_srdb2.h"
//#include "erlang_cmd.h"
//#include "erlang_listener.h"
#include "../../dprint.h"

int erlang_srdb2_ctx(db_ctx_t* ctx) {
	LM_DBG("erlang_srdb2_ctx %p %*s, %d\n",ctx,ctx->id.len,ctx->id.s, ctx->con_n);
	return 0;
};
int erlang_srdb2_con_connect(db_con_t* con) {
	LM_DBG("erlang_srdb2_con_connect %p\n",con);
	return 0;
//	return -1;
}
void erlang_srdb2_con_disconnect(db_con_t* con) {
	LM_DBG("erlang_srdb2_con_disconnect %p\n",con);
}
int erlang_srdb2_con(db_con_t* con) {
	LM_DBG("erlang_srdb2_con %p\n",con);
	DB_SET_PAYLOAD(con, NULL);
	con->connect = erlang_srdb2_con_connect;
	con->disconnect = erlang_srdb2_con_disconnect;
	return 0;
};

static unsigned char erlang_srdb2_uri_cmp(db_uri_t* uri1, db_uri_t* uri2) {
	LM_DBG("erlang_srdb2_uri_cmp %p %*s %*s %p %*s %*s\n",
		uri1, uri1->scheme.len,uri1->scheme.s,uri1->body.len,uri1->body.s,
		uri2, uri2->scheme.len,uri2->scheme.s,uri2->body.len,uri2->body.s);
	return 0;
//	return 1;
}

int erlang_srdb2_uri(db_uri_t* uri) {
	LM_DBG("erlang_srdb2_uri %p %*s %*s\n",uri, uri->scheme.len,uri->scheme.s,uri->body.len,uri->body.s);
	DB_SET_PAYLOAD(uri, NULL);
	uri->cmp = erlang_srdb2_uri_cmp;
	
	return 0;
};

int erlang_srdb2_res(db_cmd_t* cmd) {
	LM_DBG("erlang_srdb2_res %p\n",cmd);
	return 0;
};

int erlang_srdb2_fld(db_fld_t* fld, char* table) {
	LM_DBG("erlang_srdb2_fld %p %s %s\n",fld, fld->name,table);
	return 0;
};


int erlang_srdb2_cmd(db_cmd_t* cmd) {
	LM_DBG("erlang_srdb2_cmd %p\n",cmd);
	return 0;
};

int erlang_srdb2_cmd_exec(db_res_t* res, db_cmd_t* cmd) {
	LM_DBG("erlang_srdb2_cmd_exec %p %p\n",res ,cmd);
	db_fld_t* fld;
	ei_x_buff argbuf;
	int i,cnt;
	char *pbuf;
	
	LM_DBG("erlang_srdb2_cmd %p\n",cmd);
//	for(i = 0, fld = cmd->vals; !DB_FLD_EMPTY(fld) && !DB_FLD_LAST(fld[i]); i++) {
//		rv |= sb_add(&sql_buf, set_str(&tmpstr, fld[i].name));
//		if (!DB_FLD_LAST(fld[i + 1])) rv |= sb_add(&sql_buf, set_str(&tmpstr, ","));
//	}
	ei_x_new(&argbuf);
	// gen_call is 3-element tuple, first is atom $gen_call,.
	// second is {pid,ref} tuple and third is user data.
	ei_x_encode_tuple_header(&argbuf, 3);
	ei_x_encode_atom(&argbuf, "$gen_call");
	ei_x_encode_tuple_header(&argbuf, 2);
	// we are hacking erlang pid so we can have worker system pid here
//	memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
//<---->erl_pid.num=getpid();
//	ei_x_encode_pid(&argbuf, &erl_pid);
	ei_x_encode_atom(&argbuf,"pid");
//	utils_mk_ref(&(node->ec),&ref);
//	ei_x_encode_ref(&argbuf, &ref);
	ei_x_encode_atom(&argbuf,"ref");
//	erl_cmd->refn0=ref.n[0];
//	erl_cmd->refn1=ref.n[1];
//	erl_cmd->refn2=ref.n[2];
	
	//so much for pid, now encode tuple {db_op, table, [cols], [params]}
	ei_x_encode_tuple_header(&argbuf, 5);
	switch(cmd->type) {
	    case DB_PUT: ei_x_encode_atom(&argbuf,"db_put"); break;
	    case DB_DEL: ei_x_encode_atom(&argbuf,"db_del"); break;
	    case DB_GET: ei_x_encode_atom(&argbuf,"db_get"); break;
	    case DB_UPD: ei_x_encode_atom(&argbuf,"db_upd"); break;
	    case DB_SQL: ei_x_encode_atom(&argbuf,"db_sql"); break;
	}
	ei_x_encode_atom_len(&argbuf,cmd->table.s,cmd->table.len);
	
	for(i = 0, fld = cmd->result; !DB_FLD_LAST(fld[i]); i++) {}
	ei_x_encode_list_header(&argbuf, i);
	for(i = 0, fld = cmd->result; !DB_FLD_LAST(fld[i]); i++) {
		ei_x_encode_atom(&argbuf,fld[i].name);
	}
	ei_x_encode_empty_list(&argbuf);
	
	if(cmd->match) {
	    for(i = 0, fld = cmd->match; !DB_FLD_LAST(fld[i]); i++) {}
	    ei_x_encode_list_header(&argbuf, i);
	    for(i = 0, fld = cmd->match; !DB_FLD_LAST(fld[i]); i++) {
		ei_x_encode_tuple_header(&argbuf, 3);
		ei_x_encode_atom(&argbuf,fld[i].name);
		switch(fld[i].op) {
		    case DB_EQ:  ei_x_encode_atom(&argbuf,"="); break;
		    case DB_NE:  ei_x_encode_atom(&argbuf,"!="); break;
		    case DB_LT:  ei_x_encode_atom(&argbuf,"<"); break;
		    case DB_GT:  ei_x_encode_atom(&argbuf,">"); break;
		    case DB_LEQ: ei_x_encode_atom(&argbuf,"<="); break;
		    case DB_GEQ: ei_x_encode_atom(&argbuf,">="); break;
		}
		ei_x_encode_atom_len(&argbuf,fld[i].v.lstr.s,fld[i].v.lstr.len);
	    }
	    ei_x_encode_empty_list(&argbuf);
	} else {
	    ei_x_encode_list_header(&argbuf, 0);
	}
	if(cmd->vals) {
	    for(i = 0, fld = cmd->vals; !DB_FLD_LAST(fld[i]); i++) {}
	    ei_x_encode_list_header(&argbuf, i);
	    for(i = 0, fld = cmd->result; !DB_FLD_LAST(fld[i]); i++) {
		ei_x_encode_atom(&argbuf,fld[i].name);
	    }
	    ei_x_encode_empty_list(&argbuf);
	} else {
	    ei_x_encode_list_header(&argbuf, 0);
	}
	
//	if(ei_x_format_wo_ver(&argbuf, printbuf)!=0) {
//		LM_ERR("cannot fromat erlang binary from arg string\n");
//		goto error;
//	}
	
	i=0;
	pbuf=pkg_malloc(BUFSIZ);
	ei_s_print_term(&pbuf, argbuf.buff, &i);
	LM_DBG("message is pbuf='%s' buf.buffsz=%d buf.index=%d i=%d\n", pbuf, argbuf.buffsz,argbuf.index,i );
	pkg_free(pbuf);
	
	return 0;
};

int erlang_srdb2_cmd_first(db_res_t* res) {
	LM_DBG("erlang_srdb2_cmd_first %p\n",res);
	return 1;
};

int erlang_srdb2_cmd_next(db_res_t* res) {
	LM_DBG("erlang_srdb2_cmd_next %p\n",res);
	return 1;
};

int erlang_srdb2_getopt(db_cmd_t* cmd, char* optname, va_list ap) {
	LM_DBG("erlang_srdb2_getopt %p %s\n",cmd, optname);
	return 0;
};

int erlang_srdb2_setopt(db_cmd_t* cmd, char* optname, va_list ap) {
	LM_DBG("erlang_srdb2_setopt %p %s\n",cmd,optname);
	return 0;
};

