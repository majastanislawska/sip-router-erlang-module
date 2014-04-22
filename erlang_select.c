
#include "erlang_mod.h"
#include "erlang_select.h"

int sel_erl(str* res, select_t* s, struct sip_msg* msg) {
	LM_DBG("dummy");
	return 0;
}

int sel_erl_scan(int* index, select_t* s) {
	int retcode,ei_type,size;
	int i,j;
	LM_DBG("sel_erl_get_type: start\n");
	for(i=1;i< s->n-1;i++) {  // skip first element(@erlang) and skip last. calling function will handle last
		switch(s->params[i].type) {
			case SEL_PARAM_INT:
				LM_DBG("params %d int=%d\n",i, s->params[i].v.i);
				if(s->params[i].v.i > size) {
					LM_ERR("sel_erl_scan: index in select param %d greater than element size\n",i);
					return -1;
				}
				for(j=0; j < s->params[i].v.i; j++) {
					retcode=ei_skip_term(lastterm.buff, index);
					if (retcode < 0) {
						LM_ERR("sel_erl_scan: error while skipping element\n");
						return -1;
					}
					LM_DBG("skipping %d index=%d\n",i, *index);
				}
				break;
			case SEL_PARAM_STR:
				LM_DBG("params %d str=%*s\n",i,s->params[i].v.s.len,s->params[i].v.s.s);
				retcode=ei_get_type_internal(lastterm.buff, index, &ei_type, &size);
				if (retcode < 0) {
					LM_ERR("sel_erl_scan: error getting type for element\n");
					return -1;
				}
				switch(ei_type) {
					case ERL_SMALL_TUPLE_EXT:
					case ERL_LARGE_TUPLE_EXT:
						LM_DBG("sel_erl_scan: got tuple %d\n", size);
						retcode=ei_decode_tuple_header(lastterm.buff, index, &size);
						if (retcode < 0) {
							LM_ERR("sel_erl_scan: error decoding tuple\n");
							return -1;
						}
						break;
					case ERL_LIST_EXT:
						LM_DBG("sel_erl_scan: got list %d\n", size);
						retcode=ei_decode_list_header(lastterm.buff, index, &size);
						if (retcode < 0) {
							LM_ERR("sel_erl_scan: error decoding tuple\n");
							return -1;
						}
						break;
					default:
						LM_ERR("sel_erl_scan: this element is not nested\n");
				}
				break;
			default:
				LM_DBG("params %d div or ptr\n",i);
		}
	}
	LM_DBG("index on exit %d\n",*index);
	return 0;
}

int sel_erl_print(str* res, select_t* s, struct sip_msg* msg) {
	int i=0,j;
	char *pbuf=NULL;
	static char termprintbuf[BUFSIZ];

	int index=0, ei_type,size, retcode;

	retcode=sel_erl_scan(&index,s);
	if (retcode < 0) {
		LM_ERR("sel_erl_get_type: error while trying to reach element\n");
		return -1;
	}
	LM_DBG("sel_erl_value\n");
	res->len=ei_s_print_term(&pbuf, lastterm.buff, &index);
	if(res->len <0) {
		free(pbuf);
		LM_DBG("error printing term\n");
		return -1;
	}
	LM_DBG("sel_erl_print: message is pbuf='%s' buf.buffsz=%d buf.index=%d i=%d\n", pbuf, lastterm.buffsz,lastterm.index, i );
	memcpy(termprintbuf,pbuf,res->len);
	free(pbuf);
	res->s=termprintbuf;
	return 0;
}
int sel_erl_value(str* res, select_t* s, struct sip_msg* msg) {
	int i=0,j,size=BUFSIZ;
	double f;
	char *pbuf=NULL;
	static char termprintbuf[BUFSIZ];

	int index=0, ei_type, retcode;

	retcode=sel_erl_scan(&index,s);
	if (retcode < 0) {
		LM_ERR("sel_erl_get_type: error while trying to reach element\n");
		return -1;
	}
	retcode=ei_get_type_internal(lastterm.buff, &index, &ei_type, &i);
	if (retcode < 0) {
		LM_ERR("sel_erl_gettype: error getting type for element\n");
		return -1;
	}
	LM_DBG("sel_erl_value\n");
	switch(ei_type) {
		case ERL_SMALL_INTEGER_EXT:
		case ERL_INTEGER_EXT:
			retcode=ei_decode_long(lastterm.buff, &index, &i);
			if(retcode < 0) {
				return -1;
			}
			retcode = snprintf(termprintbuf, BUFSIZ, "%d", i);
			if (retcode < 0 || retcode >= size) {
				return -1;
			}
			res->len=retcode;
			res->s=termprintbuf;
			LM_DBG("decoded interger %d\n",i);
			break;
		case ERL_FLOAT_EXT:
		case NEW_FLOAT_EXT:
			retcode=ei_decode_double(lastterm.buff, &index, &f);
			if(retcode < 0) {
				return -1;
			}
			retcode = snprintf(termprintbuf, BUFSIZ, "%f", f);
			if (retcode < 0 || retcode >= size) {
				return -1;
			}
			res->len=retcode;
			res->s=termprintbuf;
			LM_DBG("decoded float %f\n",f);
			break;
		case ERL_ATOM_EXT:
		case ERL_SMALL_ATOM_EXT:
		case ERL_ATOM_UTF8_EXT:
		case ERL_SMALL_ATOM_UTF8_EXT:
			retcode=ei_decode_atom(lastterm.buff, &index, termprintbuf);
			if(retcode < 0) {
				return -1;
			}
			res->len=strlen(termprintbuf);
			res->s=termprintbuf;
			LM_DBG("decoded atom %s\n",termprintbuf);
			break;
		case ERL_STRING_EXT:
			retcode=ei_decode_string(lastterm.buff, &index, termprintbuf);
			if(retcode < 0) {
				return -1;
			}
			res->len=strlen(termprintbuf);
			res->s=termprintbuf;
			LM_DBG("decoded string %s\n",termprintbuf);
			break;
		case ERL_SMALL_TUPLE_EXT:
		case ERL_LARGE_TUPLE_EXT:
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
			LM_ERR("sel_erl_value: type not suitable to return simple value\n");
			return -1;
	}
	return 0;
}

#define STR_STATIC_RES(v) res->s=v; res->len=sizeof(v) - 1;
int sel_erl_get_type(str* res, select_t* s, struct sip_msg* msg) {
	int index=0, ei_type,size, retcode;

	retcode=sel_erl_scan(&index,s);
	if (retcode < 0) {
		LM_ERR("sel_erl_get_type: error while trying to reach element\n");
		return -1;
	}
	retcode=ei_get_type_internal(lastterm.buff, &index, &ei_type, &size);
	if (retcode < 0) {
		LM_ERR("sel_erl_gettype: error getting type for element\n");
		return -1;
	}
	switch(ei_type) {
	    case ERL_SMALL_INTEGER_EXT:
	    case ERL_INTEGER_EXT:		STR_STATIC_RES("integer"); break;
	    case ERL_FLOAT_EXT:
	    case NEW_FLOAT_EXT:			STR_STATIC_RES("float"); break;
	    case ERL_ATOM_EXT:
	    case ERL_SMALL_ATOM_EXT:
	    case ERL_ATOM_UTF8_EXT:
	    case ERL_SMALL_ATOM_UTF8_EXT:	STR_STATIC_RES("atom"); break;
	    case ERL_STRING_EXT:		STR_STATIC_RES("string"); break;
	    case ERL_SMALL_TUPLE_EXT:
	    case ERL_LARGE_TUPLE_EXT:		STR_STATIC_RES("tuple"); break;
	    case ERL_REFERENCE_EXT:
	    case ERL_NEW_REFERENCE_EXT:		STR_STATIC_RES("reference"); break;
	    case ERL_PORT_EXT:			STR_STATIC_RES("erlang_port"); break;
	    case ERL_PID_EXT:			STR_STATIC_RES("erlang_pid"); break;
	    case ERL_NIL_EXT: 			STR_STATIC_RES("nil"); break;
	    case ERL_LIST_EXT:			STR_STATIC_RES("list"); break;
	    case ERL_BINARY_EXT:		STR_STATIC_RES("binary"); break;
	    case ERL_SMALL_BIG_EXT:		STR_STATIC_RES("small_big"); break;
	    case ERL_LARGE_BIG_EXT:		STR_STATIC_RES("large_big"); break;
	    case ERL_NEW_FUN_EXT:		STR_STATIC_RES("nil"); break;
	    case ERL_FUN_EXT:			STR_STATIC_RES("fun"); break;
	    default:
	        LM_ERR("erlang_srdb1_query: don't know how to handle element\n");
	}
	return 0;
}
//int sel_erl_get_size(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_int(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_string(str* res, select_t* s, struct sip_msg* msg);
//int sel_erl_get_float(str* res, select_t* s, struct sip_msg* msg);
