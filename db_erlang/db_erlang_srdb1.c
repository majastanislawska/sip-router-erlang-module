#include <stdio.h>
#include <string.h>
#include "../../mem/mem.h"
#include "../../dprint.h"
#include "../../lib/srdb1/db_query.h"
#include "../../lib/srdb1/db_ut.h"
#include "../../lib/srdb1/db_con.h"

//temp
#include "../../lib/srdb1/db_pool.h"
//temp
#include "../../lib/srdb1/db_id.h"

#include "db_erlang_mod.h"
#include "db_erlang_srdb1.h"

/**
 * Store the name of table that will be used by subsequent database functions
 * \param _h database handle
 * \param _t table name
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_use_table(db1_con_t* _h, const str* _t)
{
	LM_DBG("erlang_srdb1_use_table %.*s\n", _t->len, _t->s);
	return db_use_table(_h, _t);
}

//temporary
void *erlang_srdb1_new_connection(const struct db_id* id) 
{
	void *ptr;
//	int n = 0;
//	char tmp[50];
//	struct erl_con *con;
	LM_DBG("erlang_srdb1_new_connection\n");
	LM_DBG(" scheme   %s\n",id->scheme);
	LM_DBG(" username %s\n",id->username);
	LM_DBG(" password %s\n",id->password);
	LM_DBG(" host %s\n",id->host);
	LM_DBG(" port %i\n",id->port);
	LM_DBG(" database %s\n",id->database);
	LM_DBG(" pid %i\n",id->pid);
	LM_DBG(" poolid %i\n",id->poolid);
//	con=pkg_malloc(sizeof(struct erl_con));
//	if (!con) {
//		LM_DBG("erlang_srdb1_new_connection pgg_malloc failed\n");
//		return 0;
//	}
//	if (ei_connect_init(&(con->ec), "kamailio", "cookie", n++) < 0) {
//		LM_ERR("erlang_srdb1_new_connection ei_connect_init error\n");
////		LM_ERR("erlang_srdb1_new_connection ei_connect_init error %d\n",erl_errno);
//		pkg_free(con);
//		return 0;
//	}
//	snprintf(tmp,50,"%s@%s",id->username,id->host);
//	con->fd = ei_connect(&(con->ec), tmp);
//	if(con->fd <0) {
//		LM_ERR("erlang_srdb1_new_connection ei_connect error\n");
////		LM_ERR("erlang_srdb1_new_connection ei_connect error %d\n",erl_errno);
//		pkg_free(con);
//		return 0;
//	}
//	LM_DBG(" erlang_srdb1_new_connection finishes successfully ec=%p fd=%d\n",&(con->ec),con->fd);
	ptr=pkg_malloc(64);
	LM_DBG("erlang_new_connection %p \n",ptr);
	return ptr;
}
/**
 * Initialize the database module.
 * No function should be called before this
 * \param _url URL used for initialization
 * \return zero on success, negative value on failure
 */
db1_con_t* erlang_srdb1_init(const str* _url)
{
	LM_DBG("erlang_srdb1_init %.*s \n", _url->len, _url->s);
	return db_do_init(_url, (void *)erlang_srdb1_new_connection);
}

//temporary
void erlang_srdb1_free_connection(struct pool_con* con) {
	LM_DBG("erlang_free_connection %p \n",con);
	pkg_free(con);
	return;
}
/**
 * Shut down the database module.
 * No function should be called after this
 * \param _h handle to the closed connection
 * \return zero on success, negative value on failure
 */
void erlang_srdb1_close(db1_con_t* _h)
{
	LM_DBG("erlang_erlang_srdb1_close %p \n",_h);
	db_do_close(_h, erlang_srdb1_free_connection);
}



/**
 * Query a table for specified rows.
 * \param _h structure representing database connection
 * \param _k key names
 * \param _op operators
 *\param  _v values of the keys that must match
 * \param _c column names to return
 * \param _n number of key=values pairs to compare
 * \param _nc number of columns to return
 * \param _o order by the specified column
 * \param _r pointer to a structure representing the result
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_query(const db1_con_t* _h, const db_key_t* _k, const db_op_t* _op,
	     const db_val_t* _v, const db_key_t* _c, const int _n, const int _nc,
	     const db_key_t _o, db1_res_t** _r) {
	ei_x_buff argbuf;
	int i;
	char *pbuf;
	
	LM_DBG("erlang_srdb1_query %p %p\n",_r, *_r);
	if (!_h || !_r) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	*_r=NULL;
	LM_DBG("erlang_srdb1_query table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	// gen_call is 3-element tuple, first is atom $gen_call,
	// second is {pid,ref} tuple and third is user data.
	ei_x_encode_tuple_header(&argbuf, 3);
	ei_x_encode_atom(&argbuf, "$gen_call");
	ei_x_encode_tuple_header(&argbuf, 2);
	// we are hacking erlang pid so we can have worker system pid here
//	memcpy(&erl_pid,ei_self(&(node->ec)),sizeof(erlang_pid));
//	erl_pid.num=getpid();
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
//	switch(cmd->type) {
//	    case DB_PUT: ei_x_encode_atom(&argbuf,"db_put"); break;
//	    case DB_DEL: ei_x_encode_atom(&argbuf,"db_del"); break;
//	    case DB_GET: ei_x_encode_atom(&argbuf,"db_get"); break;
//	    case DB_UPD: ei_x_encode_atom(&argbuf,"db_upd"); break;
//	    case DB_SQL: ei_x_encode_atom(&argbuf,"db_sql"); break;
//	}
	ei_x_encode_atom(&argbuf,"db_get");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	if(_c) {
	    ei_x_encode_list_header(&argbuf, _nc);
	    for(i = 0; i < _nc; i++) {
		ei_x_encode_atom_len(&argbuf,_c[i]->s,_c[i]->len);
	    }
	    ei_x_encode_empty_list(&argbuf);
	} else {
	    ei_x_encode_atom(&argbuf,"all");
	}
	if(_k) {
	    ei_x_encode_list_header(&argbuf, _n);
	    for(i = 0; i < _n; i++) {
		db_val_t *vv;
		ei_x_encode_tuple_header(&argbuf, 3);
		ei_x_encode_atom_len(&argbuf,_k[i]->s,_k[i]->len);
		if (_op) {
		    ei_x_encode_atom(&argbuf,_op[i]);
		} else {
		    ei_x_encode_atom(&argbuf,"=");
		}
		vv=&(_v[i]);
		switch(VAL_TYPE(vv)) {
		    case DB1_INT:
			ei_x_encode_ulong(&argbuf, VAL_INT(vv));
			break;
		    case DB1_BIGINT:
			ei_x_encode_longlong(&argbuf, VAL_BIGINT(vv));
			break;
		    case DB1_DOUBLE:
			ei_x_encode_double(&argbuf, VAL_DOUBLE(vv));
			break;
		    case DB1_STRING:
			ei_x_encode_string(&argbuf,VAL_STRING(vv));
			break;
		    case DB1_STR:
			ei_x_encode_string_len(&argbuf,VAL_STR(vv).s,VAL_STR(vv).len);
			break;
//		    case DB1_DATETIME: 
//			ei_x_encode_string(&argbuf,);
//			break;
//		    case DB1_BLOB:
//			ei_x_encode_binary(&argbuf,VAL_BLOB(vv));
//			break;
//		    case DB1_BITMAP:
//			ei_x_encode_binary(&argbuf,&VAL_BITMAP(vv));
//			 break;
		}
	    }
	    ei_x_encode_empty_list(&argbuf);
	} else {
	    ei_x_encode_list_header(&argbuf, 0);
	}
//	if(cmd->vals) {
//	    for(i = 0, fld = cmd->vals; !DB_FLD_LAST(fld[i]); i++) {}
//	    ei_x_encode_list_header(&argbuf, i);
//	    for(i = 0, fld = cmd->result; !DB_FLD_LAST(fld[i]); i++) {
//		ei_x_encode_atom(&argbuf,fld[i].name);
//	    }
//	    ei_x_encode_empty_list(&argbuf);
//	} else {
	    ei_x_encode_list_header(&argbuf, 0);
//	}

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
}

/**
 * Release a result set from memory.
 * \param _h handle to the database
 * \param _r result set that should be freed
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_free_result(db1_con_t* _h, db1_res_t* _r)
{
     LM_DBG("erlang_srdb1_free_result\n");
     if ((!_h) || (!_r)) {
	     LM_ERR("invalid parameter value\n");
	     return -1;
     }

     if (db_free_result(_r) < 0) {
	     LM_ERR("unable to free result structure\n");
	     return -1;
     }
//     mysql_free_result(CON_RESULT(_h));
//     CON_RESULT(_h) = 0;
     return 0;
}

/**
 * \brief Gets a partial result set, fetch rows from a result
 *
 * Gets a partial result set, fetch a number of rows from a database result.
 * This function initialize the given result structure on the first run, and
 * fetches the nrows number of rows. On subsequenting runs, it uses the
 * existing result and fetches more rows, until it reaches the end of the
 * result set. Because of this the result needs to be null in the first
 * invocation of the function. If the number of wanted rows is zero, the
 * function returns anything with a result of zero.
 * \param _h structure representing the database connection
 * \param _r pointer to a structure representing the result
 * \param nrows number of fetched rows
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_fetch_result(const db1_con_t* _h, db1_res_t** _r, const int nrows)
{
	int rows, i, code;

	LM_DBG("erlang_srdb1_fetch_result\n");
	if (!_h || !_r || nrows < 0) {
		LM_ERR("Invalid parameter value\n");
		return -1;
	}

	/* exit if the fetch count is zero */
	if (nrows == 0) {
		db_free_result(*_r);
		*_r = 0;
		return 0;
	}

	if(*_r==0) {
		/* Allocate a new result structure */
		*_r = db_new_result();
		if (*_r == 0) {
			LM_ERR("no memory left\n");
			return -2;
		}

	} else {
		/* free old rows */
		if(RES_ROWS(*_r)!=0)
			db_free_rows(*_r);
		RES_ROWS(*_r) = 0;
		RES_ROW_N(*_r) = 0;
	}

	/* determine the number of rows remaining to be processed */
	rows = RES_NUM_ROWS(*_r) - RES_LAST_ROW(*_r);

	/* If there aren't any more rows left to process, exit */
	if(rows<=0)
		return 0;

	/* if the fetch count is less than the remaining rows to process                 */
	/* set the number of rows to process (during this call) equal to the fetch count */
	if(nrows < rows)
		rows = nrows;

	RES_ROW_N(*_r) = rows;

	LM_DBG("converting row %d of %d count %d\n", RES_LAST_ROW(*_r),
			RES_NUM_ROWS(*_r), RES_ROW_N(*_r));

	RES_ROWS(*_r) = (struct db_row*)pkg_malloc(sizeof(db_row_t) * rows);
	if (!RES_ROWS(*_r)) {
		LM_ERR("no memory left\n");
		return -5;
	}

	/* update the total number of rows processed */
	RES_LAST_ROW(*_r) += rows;
	return 0;
}

/**
 * Execute a raw SQL query.
 * \param _h handle for the database
 * \param _s raw query string
 * \param _r result set for storage
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_raw_query(const db1_con_t* _h, const str* _s, db1_res_t** _r)
{
//	return db_do_raw_query(_h, _s, _r, db_mysql_submit_query,
//	db_mysql_store_result);
	LM_DBG("erlang_srdb1_raw_query\n");
	return 0;
}


/**
 * Insert a row into a specified table.
 * \param _h structure representing database connection
 * \param _k key names
 * \param _v values of the keys
 * \param _n number of key=value pairs
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_insert(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v, const int _n)
{
//	return db_do_insert(_h, _k, _v, _n, db_mysql_val2str,
//	db_mysql_submit_query);
	LM_DBG("erlang_srdb1_insert\n");
	return 0;
}


/**
 * Delete a row from the specified table
 * \param _h structure representing database connection
 * \param _k key names
 * \param _o operators
 * \param _v values of the keys that must match
 * \param _n number of key=value pairs
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_delete(const db1_con_t* _h, const db_key_t* _k, const db_op_t* _o,
	const db_val_t* _v, const int _n)
{
//	return db_do_delete(_h, _k, _o, _v, _n, db_mysql_val2str,
//	db_mysql_submit_query);
	LM_DBG("erlang_srdb1_delete\n");
	return 0;
}


/**
 * Update some rows in the specified table
 * \param _h structure representing database connection
 * \param _k key names
 * \param _o operators
 * \param _v values of the keys that must match
 * \param _uk updated columns
 * \param _uv updated values of the columns
 * \param _n number of key=value pairs
 * \param _un number of columns to update
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_update(const db1_con_t* _h, const db_key_t* _k, const db_op_t* _o, 
	const db_val_t* _v, const db_key_t* _uk, const db_val_t* _uv, const int _n, 
	const int _un)
{
//	return db_do_update(_h, _k, _o, _v, _uk, _uv, _n, _un, db_mysql_val2str,
//	db_mysql_submit_query);
	LM_DBG("erlang_srdb1_update\n");
	return 0;

}


/**
 * Just like insert, but replace the row if it exists.
 * \param _h database handle
 * \param _k key names
 * \param _v values of the keys that must match
 * \param _n number of key=value pairs
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_replace(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v, const int _n, const int _m)
{
//	return db_do_replace(_h, _k, _v, _n, db_mysql_val2str,
//	db_mysql_submit_query);
	LM_DBG("erlang_srdb1_replace\n");
	return 0;
}


/**
 * Returns the last inserted ID.
 * \param _h database handle
 * \return returns the ID as integer or returns 0 if the previous statement
 * does not use an AUTO_INCREMENT value.
 */
int erlang_srdb1_last_inserted_id(const db1_con_t* _h)
{
	if (!_h) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
//	return mysql_insert_id(CON_CONNECTION(_h));
	LM_DBG("erlang_srdb1_last_inserted_id\n");
	return -1;

}


/**
 * Returns the affected rows of the last query.
 * \param _h database handle
 * \return returns the affected rows as integer or -1 on error.
 */
int erlang_srdb1_affected_rows(const db1_con_t* _h)
{
	if (!_h) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
//	return (int)mysql_affected_rows(CON_CONNECTION(_h));
	LM_DBG("erlang_srdb1_affected_rows\n");
	return 0;

}


/**
  * Insert a row into a specified table, update on duplicate key.
  * \param _h structure representing database connection
  * \param _k key names
  * \param _v values of the keys
  * \param _n number of key=value pairs
 */
 int erlang_srdb1_insert_update(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v,
	const int _n)
 {
//	int off, ret;
//	static str  sql_str;
 
	if ((!_h) || (!_k) || (!_v) || (!_n)) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	LM_DBG("erlang_srdb1_insert_update\n");
	return 0;
}


/**
 * Insert delayed a row into a specified table.
 * \param _h structure representing database connection
 * \param _k key names
 * \param _v values of the keys
 * \param _n number of key=value pairs
 * \return zero on success, negative value on failure
 */
int erlang_srdb1_insert_delayed(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v, const int _n)
{
//	return db_do_insert_delayed(_h, _k, _v, _n, db_mysql_val2str,
//	db_mysql_submit_query);
	LM_DBG("erlang_srdb1_insert_delayed\n");
	return 0;
}

