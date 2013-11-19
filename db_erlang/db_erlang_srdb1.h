#ifndef ERLANG_SRDB1_H
#define ERLANG_SRDB1_H

#include "../../lib/srdb1/db.h"
#include "../../lib/srdb1/db_con.h"
#include "../../lib/srdb1/db_res.h"
#include "../../lib/srdb1/db_key.h"
#include "../../lib/srdb1/db_op.h"
#include "../../lib/srdb1/db_val.h"
#include "../../str.h"

int erlang_srdb1_use_table(db1_con_t* _h, const str* _t);

db1_con_t* erlang_srdb1_init(const str* _sqlurl);

void erlang_srdb1_close(db1_con_t* _h);

int erlang_srdb1_query(const db1_con_t* _h, const db_key_t* _k, const db_op_t* _op,
	const db_val_t* _v, const db_key_t* _c, const int _n, const int _nc,
	const db_key_t _o, db1_res_t** _r);

int erlang_srdb1_fetch_result(const db1_con_t* _h, db1_res_t** _r, const int nrows);

int erlang_srdb1_raw_query(const db1_con_t* _h, const str* _s, db1_res_t** _r);

int erlang_srdb1_free_result(db1_con_t* _h, db1_res_t* _r);

int erlang_srdb1_insert(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v, const int _n);

int erlang_srdb1_delete(const db1_con_t* _h, const db_key_t* _k, const 
	db_op_t* _o, const db_val_t* _v, const int _n);

int erlang_srdb1_update(const db1_con_t* _h, const db_key_t* _k, const db_op_t* _o,
	const db_val_t* _v, const db_key_t* _uk, const db_val_t* _uv, const int _n,
	const int _un);

int erlang_srdb1_replace(const db1_con_t* handle, const db_key_t* keys, const db_val_t* vals, const int n);

int erlang_srdb1_last_inserted_id(const db1_con_t* _h);

int erlang_srdb1_insert_update(const db1_con_t* _h, const db_key_t* _k, const db_val_t* _v,
	const int _n);

int erlang_srdb1_insert_delayed(const db1_con_t* _h, const db_key_t* _k,
		const db_val_t* _v, const int _n);

int erlang_srdb1_affected_rows(const db1_con_t* _h);

#endif /* ERLANG_SRDB1_H */
