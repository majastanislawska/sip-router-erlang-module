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

extern struct erlang_binds erl_bind;

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
	struct erlang_connection *con;

	if (!id) {
		LM_ERR("invalid parameter value\n");
		return 0;
	}
	con=pkg_malloc(sizeof(struct erlang_connection));
	if (!con) {
		LM_DBG("erlang_srdb1_new_connection pkg_malloc failed\n");
		return 0;
	}
	memset(con, 0, sizeof(*con));
	con->hdr.ref = 1;
	con->hdr.id = (struct db_id*)id;
	con->con.s=id->host;
	con->con.len=strlen(id->host);
	con->regname.s=id->database;
	con->regname.len=strlen(id->database);
	LM_DBG("erlang_new_connection %p \n",con);
	return con;
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
void erlang_srdb1_free_connection(struct erlang_connection* con) {
	LM_DBG("erlang_free_connection %p \n",con);
	free_db_id(con->hdr.id);
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

int srdb1_encode_kv(int tupsize,const db_key_t* _k, const db_op_t* _op, const db_val_t* _v,
				const int _n, ei_x_buff *argbuf) {
	int i;
	struct tm* tt;
	time_t t_t;

	if(_k) {
	    ei_x_encode_list_header(argbuf, _n);
	    for(i = 0; i < _n; i++) {
		db_val_t *vv;
		ei_x_encode_tuple_header(argbuf, tupsize);
		ei_x_encode_atom_len(argbuf,_k[i]->s,_k[i]->len);
		if(tupsize == 3 ) {
		    if (_op) {
			ei_x_encode_atom(argbuf,_op[i]);
		    } else {
			ei_x_encode_atom(argbuf,"=");
		    }
		}
		vv=&(_v[i]);
		if (VAL_NULL(vv)) {
		    ei_x_encode_atom(argbuf,"undefined");
		} else {
		    switch(VAL_TYPE(vv)) {
			case DB1_INT:
			    ei_x_encode_ulong(argbuf, VAL_INT(vv));
			    break;
			case DB1_BIGINT:
			    ei_x_encode_longlong(argbuf, VAL_BIGINT(vv));
			    break;
			case DB1_DOUBLE:
			    ei_x_encode_double(argbuf, VAL_DOUBLE(vv));
			    break;
			case DB1_STRING:
			    ei_x_encode_string(argbuf,VAL_STRING(vv));
			    break;
			case DB1_STR:
			    ei_x_encode_string_len(argbuf,VAL_STR(vv).s,VAL_STR(vv).len);
			    break;
			case DB1_DATETIME:
			    t_t=VAL_TIME(vv);
			    tt= localtime(&t_t);
			    ei_x_encode_tuple_header(argbuf, 2);
			    ei_x_encode_tuple_header(argbuf, 3);
			    ei_x_encode_long(argbuf, tt->tm_year + 1900);
			    ei_x_encode_long(argbuf, tt->tm_mon +1);
			    ei_x_encode_long(argbuf, tt->tm_mday);
			    ei_x_encode_tuple_header(argbuf, 3);
			    ei_x_encode_long(argbuf, tt->tm_hour);
			    ei_x_encode_long(argbuf, tt->tm_min);
			    ei_x_encode_long(argbuf, tt->tm_sec);
			    break;
			case DB1_BLOB:
			    ei_x_encode_binary(argbuf,VAL_BLOB(vv).s,VAL_BLOB(vv).len);
			    break;
			case DB1_BITMAP:
			    ei_x_encode_ulong(argbuf,VAL_BITMAP(vv));
			    break;
		    }
		}
	    }
	    ei_x_encode_empty_list(argbuf);
	} else {
	    ei_x_encode_list_header(argbuf, 0);
	}
	return 0;
}
int srdb1_encode_k(const db_key_t* _k, const db_op_t* _op, const db_val_t* _v,
				const int _n, ei_x_buff *argbuf) {
	return srdb1_encode_kv(3, _k, _op, _v, _n, argbuf);
}

int srdb1_encode_v(const db_key_t* _k, const db_val_t* _v,
				const int _n, ei_x_buff *argbuf) {
	return srdb1_encode_kv(2, _k, NULL, _v, _n, argbuf);
}

int srdb1_encode_c(const db_key_t* _c, const int _nc, ei_x_buff *argbuf){
	int i;

	if(_c) {
	    ei_x_encode_list_header(argbuf, _nc);
	    for(i = 0; i < _nc; i++) {
		ei_x_encode_atom_len(argbuf,_c[i]->s,_c[i]->len);
	    }
	    ei_x_encode_empty_list(argbuf);
	} else {
	    ei_x_encode_atom(argbuf,"all");
	}
	return 0;
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
	ei_x_buff argbuf,retbuf;
	int retcode,i,j,x;
	int n_cols,n_rows,len;
	db1_res_t *res;
	db_row_t *rows = NULL, *row;
	db_val_t *val;
	char atom[MAXATOMLEN], *p;
	ei_term term;
	int ei_type,size;
	str *sname;

	if (!_h || !_r) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	*_r=NULL;
	LM_DBG("erlang_srdb1_query table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	//encode tuple {db_op, table, [cols], [params]}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"select");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	srdb1_encode_c(_c, _nc, &argbuf);
	srdb1_encode_k(_k, _op, _v, _n, &argbuf);
//	ei_x_encode_atom_len(&argbuf,_o->s,_o->len);
	ei_x_encode_list_header(&argbuf, 0);

	retcode=erl_bind.do_erlang_call(&(CON_ERLANG(_h)->con),&(CON_ERLANG(_h)->regname), &argbuf, &retbuf);
	ei_x_free(&argbuf);
	if (retcode<0) {
		if(retbuf.buff) shm_free(retbuf.buff);
		return retcode;
	}
	// we have a tuple there:
	ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &i);
	x=retbuf.index;
	ei_skip_term(retbuf.buff, &x);
	LM_DBG("erlang_srdb1_query: position of end of field list should be %d\n",x);
	//first is list of 5-element tuples containing name and type of field
	ei_decode_list_header(retbuf.buff, &(retbuf.index), &n_cols);
	LM_DBG("erlang_srdb1_query: length -f field_list is %d\n",n_cols);
	res=db_new_result();
	if (db_allocate_columns(res, n_cols) != 0) {
		LM_ERR("erlang_srdb1_query: db_allocate_columns failed\n");
		goto error;
	}
	RES_COL_N(res) = n_cols;
	for(i=0; i < n_cols; i++) {
		x=retbuf.index;
		ei_skip_term(retbuf.buff, &x);
		LM_DBG("erlang_srdb1_query: position of end of this field should be %d\n",x);
		ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &j);
		if( j!=5) LM_ERR("erlang_srdb1_query name&type list element tuple is not 5\n");
		ei_decode_atom(retbuf.buff, &(retbuf.index), atom);  //1  name
		len=strlen(atom);
		sname = (str*)pkg_malloc(sizeof(str)+len+1);
		if (!sname) {
			LM_ERR("no private memory left\n");
			goto error;
		}
		sname->len = len;
		sname->s = (char*)sname + sizeof(str);
		memcpy(sname->s, atom, len);
		sname->s[len] = '\0';
		RES_NAMES(res)[i] = sname;
		LM_DBG("decoded header %d, fieled 1: %s\n",i,atom);
		ei_decode_atom(retbuf.buff, &(retbuf.index), atom); //2 type atom
		if(strcmp("int",atom)==0) { RES_TYPES(res)[i]=DB1_INT; }
		if(strcmp("string",atom)==0) { RES_TYPES(res)[i]=DB1_STRING; }
		if(strcmp("float",atom)==0) { RES_TYPES(res)[i]=DB1_DOUBLE; }
		if(strcmp("datetime",atom)==0) { RES_TYPES(res)[i]=DB1_DATETIME; }
//		if(strcmp("string",atom)==0) { RES_TYPES(res)[i]=DB1_BLOB; }
		ei_skip_term(retbuf.buff, &(retbuf.index));  //3 size (ignored)
		ei_skip_term(retbuf.buff, &(retbuf.index));  //4 default value (ignored)
		ei_skip_term(retbuf.buff, &(retbuf.index));  //3 null status (ignored)
		LM_DBG("end of %d record: %d\n",i,retbuf.index);
	}
	ei_decode_ei_term(retbuf.buff, &(retbuf.index), &term); // List tail,
	LM_DBG("erlang_srdb1_query: position after scanning is %d\n",retbuf.index);
	//now rows, list of tuples
	ei_decode_list_header(retbuf.buff, &(retbuf.index), &n_rows);
	LM_DBG("erlang_srdb1_query values list size is %d\n",n_rows);
	if (n_rows<=0) {
		LM_DBG("erlang_srdb1_query no rows returned\n");
		RES_ROWS(res) = NULL;
		RES_NUM_ROWS(res)=0;
		*_r=res;
		return 0;
	}
	RES_NUM_ROWS(res)=n_rows;
	rows = pkg_realloc(rows, sizeof(db_row_t) * n_rows);
	if (rows == NULL) {
		LM_ERR("erlang_srdb1_query: pkg_realloc rows failed\n");
		goto error;
	}
	RES_ROWS(res) = rows;
	for(i=0; i < n_rows; i++) {
		RES_ROW_N(res)=i+1;
		row = &RES_ROWS(res)[i];
		if (db_allocate_row(res, row) != 0) {
			LM_ERR("erlang_srdb1_query: db_allocate_row failed for row %d\n",i);
			goto error;
		}
		ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &j);
		if(j!=n_cols) {
			LM_ERR("erlang_srdb1_query: mismatch:values list element tuple size is %d n_cols from header was %d\n",j, n_cols);
		}
		for (j = 0, val = ROW_VALUES(row); j < RES_COL_N(res); j++, val++) {
			VAL_TYPE(val) = RES_TYPES(res)[j];
			VAL_NULL(val) = 0;
			VAL_FREE(val) = 0;
			retcode=ei_get_type_internal(retbuf.buff, &(retbuf.index), &ei_type, &size);
			if (retcode < 0) {
				LM_ERR("erlang_srdb1_query: error getting type for element %d %d\n",i,j);
				goto error;
			}
			LM_DBG("erlang_srdb1_query: element %d %d ei_type=%d size=%d\n",i,j,ei_type, size);
			switch(ei_type) {
				case ERL_SMALL_INTEGER_EXT:
				case ERL_INTEGER_EXT:
					retcode=ei_decode_long(retbuf.buff, &(retbuf.index), &VAL_INT(val));
					if(retcode < 0) goto error;
					LM_DBG("decoded interger %d\n",VAL_INT(val));
					break;
				case ERL_FLOAT_EXT:
				case NEW_FLOAT_EXT:
					retcode=ei_decode_double(retbuf.buff, &(retbuf.index), &VAL_DOUBLE(val));
					if(retcode < 0) goto error;
					LM_DBG("decoded float %f\n",VAL_DOUBLE(val));
					break;
				case ERL_ATOM_EXT:
				case ERL_SMALL_ATOM_EXT:
				case ERL_ATOM_UTF8_EXT:
				case ERL_SMALL_ATOM_UTF8_EXT:
					p=pkg_malloc(size+1);
					if(!p) { LM_ERR("erlang_srdb1_query: no memory\n"); goto error; }
					retcode=ei_decode_atom(retbuf.buff, &(retbuf.index), p);
					if(retcode < 0) {
						pkg_free(p);
						goto error;
					}
					LM_DBG("decoded small_atom_utf %s\n",p);
					VAL_STRING(val)=p;
					VAL_FREE(val)=1;
					break;
				case ERL_STRING_EXT:
					p=pkg_malloc(size+1);
					if(!p) { LM_ERR("erlang_srdb1_query: no memory\n"); goto error; }
					retcode=ei_decode_string(retbuf.buff, &(retbuf.index), p);
					if(retcode < 0) {
						pkg_free(p);
						goto error;
					}
					LM_DBG("decoded string %s\n",p);
					VAL_STRING(val)=p;
					VAL_FREE(val)=1;
					break;
				case ERL_SMALL_TUPLE_EXT:
				case ERL_LARGE_TUPLE_EXT:
					LM_DBG("got tuple)\n");
					if (VAL_TYPE(val)==DB1_DATETIME) {
					    struct tm tm;
					    LM_DBG("and col type is datetime\n");
					    retcode=ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &x);
					    if(retcode < 0) goto error;
					    retcode=ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &x);
					    if(retcode < 0) goto error;
					    retcode=ei_decode_long(retbuf.buff, &(retbuf.index), (long int *)&tm.tm_year);tm.tm_year -=1900;
					    if(retcode < 0) goto error;
					    retcode=ei_decode_long(retbuf.buff, &(retbuf.index), (long int *)&tm.tm_mon); tm.tm_mon -=1;
					    if(retcode < 0) goto error;
					    retcode=ei_decode_long(retbuf.buff, &(retbuf.index), (long int *)&tm.tm_mday);
					    if(retcode < 0) goto error;
					    retcode=ei_decode_tuple_header(retbuf.buff, &(retbuf.index), &x);
					    if(retcode < 0) goto error;
					    retcode=ei_decode_long(retbuf.buff, &(retbuf.index), (long int *)&tm.tm_hour);
					    if(retcode < 0) goto error;
					    retcode=ei_decode_long(retbuf.buff, &(retbuf.index), (long int *)&tm.tm_min);
					    if(retcode < 0) goto error;
					    retcode=ei_decode_long(retbuf.buff, &(retbuf.index), (long int *)&tm.tm_sec);
					    if(retcode < 0) goto error;
					    VAL_TIME(val)=mktime(&tm);
					    break;
					}
					LM_ERR("erlang_srdb1_query: got tuple but valtype is not datetime element %d in row %d in response\n",j,i);
					break;
				case ERL_REFERENCE_EXT:
				case ERL_NEW_REFERENCE_EXT:
				case ERL_PORT_EXT:
				case ERL_PID_EXT:
				case ERL_NIL_EXT:
				case ERL_LIST_EXT:
				case ERL_BINARY_EXT:
				case ERL_SMALL_BIG_EXT:
				case ERL_LARGE_BIG_EXT:
				case ERL_NEW_FUN_EXT:
				case ERL_FUN_EXT:
				default:
				    LM_ERR("erlang_srdb1_query: don't know how to handle element %d in row %d in response\n",j,i);
			}
		}
	}
	ei_decode_ei_term(retbuf.buff, &(retbuf.index), &term); // List tail,
	*_r=res;
	return 0;
error:
	if (res)
		db_free_result(res);
	LM_ERR("erlang_srdb1_query: Failed\n");
	return -1;
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
int erlang_srdb1_insert(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v, const int _n) {
	ei_x_buff argbuf;
	int retcode;

	LM_DBG("erlang_srdb1_insert\n");
	if (!_h) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	LM_DBG("erlang_srdb1_insert table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	//encode tuple {db_op, table, [cols], [keys], [vals]}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"insert");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	ei_x_encode_list_header(&argbuf, 0); //_c
	ei_x_encode_list_header(&argbuf, 0); //_k
	srdb1_encode_v(_k, _v, _n, &argbuf); //_v

	retcode=erl_bind.do_erlang_call(&(CON_ERLANG(_h)->con),&(CON_ERLANG(_h)->regname), &argbuf, NULL /*&retbuf*/);
	ei_x_free(&argbuf);
	if (retcode<0) {
//		if(retbuf.buff) shm_free(retbuf.buff);
		return retcode;
	}

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
		const db_val_t* _v, const int _n) {
	ei_x_buff argbuf;
	int retcode;

	LM_DBG("erlang_srdb1_delete\n");
	if (!_h) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	LM_DBG("erlang_srdb1_delete table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	//encode tuple {db_op, table, [cols], [keys], [vals]}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"delete");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	ei_x_encode_list_header(&argbuf, 0); //_c
	srdb1_encode_k(_k, _o, _v, _n, &argbuf); //_k
	ei_x_encode_list_header(&argbuf, 0); //_v

	retcode=erl_bind.do_erlang_call(&(CON_ERLANG(_h)->con),&(CON_ERLANG(_h)->regname), &argbuf, NULL /*&retbuf*/);
	ei_x_free(&argbuf);
	if (retcode<0) {
//		if(retbuf.buff) shm_free(retbuf.buff);
		return retcode;
	}
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
	const int _un) {

	ei_x_buff argbuf;
	int retcode;

	LM_DBG("erlang_srdb1_update\n");
	if (!_h) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	LM_DBG("erlang_srdb1_update table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	//encode tuple {db_op, table, [cols], [keys], [vals]}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"update");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	ei_x_encode_list_header(&argbuf, 0); //_c
	srdb1_encode_k(_k, _o, _v, _n, &argbuf); //_k
	srdb1_encode_v(_k, _v, _un, &argbuf); //_v

	retcode=erl_bind.do_erlang_call(&(CON_ERLANG(_h)->con),&(CON_ERLANG(_h)->regname), &argbuf, NULL /*&retbuf*/);
	ei_x_free(&argbuf);
	if (retcode<0) {
//		if(retbuf.buff) shm_free(retbuf.buff);
		return retcode;
	}

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
int erlang_srdb1_replace(const db1_con_t* _h, const db_key_t* _k, 
		const db_val_t* _v, const int _n, const int _m) {

	ei_x_buff argbuf;
	int retcode;

	LM_DBG("erlang_srdb1_replace\n");
	if (!_h) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	LM_DBG("erlang_srdb1_replace table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	//encode tuple {db_op, table, [cols], [keys], [vals]}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"replace");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	ei_x_encode_list_header(&argbuf, 0); //_c
//	ei_x_encode_list_header(&argbuf, 0); //_k
	srdb1_encode_k(_k, NULL, _v, _n, &argbuf); //_k
	srdb1_encode_v(_k, _v, _n, &argbuf); //_v

	retcode=erl_bind.do_erlang_call(&(CON_ERLANG(_h)->con),&(CON_ERLANG(_h)->regname), &argbuf, NULL /*&retbuf*/);
	ei_x_free(&argbuf);
	if (retcode<0) {
//		if(retbuf.buff) shm_free(retbuf.buff);
		return retcode;
	}
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
	return 0;

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
	ei_x_buff argbuf;
	int retcode;

	if ((!_h) || (!_k) || (!_v) || (!_n)) {
		LM_ERR("invalid parameter value\n");
		return -1;
	}
	LM_DBG("erlang_srdb1_insert_update table %.*s\n",CON_TABLE(_h)->len, CON_TABLE(_h)->s);
	ei_x_new(&argbuf);
	//encode tuple {db_op, table, [cols], [keys], [vals]}
	ei_x_encode_tuple_header(&argbuf, 5);
	ei_x_encode_atom(&argbuf,"insert_update");
	ei_x_encode_atom_len(&argbuf,CON_TABLE(_h)->s,CON_TABLE(_h)->len);

	ei_x_encode_list_header(&argbuf, 0); //_c
//	ei_x_encode_list_header(&argbuf, 0); //_k
	srdb1_encode_k(_k, NULL, _v, _n, &argbuf); //_k
	srdb1_encode_v(_k, _v, _n, &argbuf); //_v

	retcode=erl_bind.do_erlang_call(&(CON_ERLANG(_h)->con),&(CON_ERLANG(_h)->regname), &argbuf, NULL /*&retbuf*/);
	ei_x_free(&argbuf);
	if (retcode<0) {
//		if(retbuf.buff) shm_free(retbuf.buff);
		return retcode;
	}
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

