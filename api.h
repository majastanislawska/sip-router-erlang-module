#ifndef ERLANG_API_H
#define ERLANG_API_H
#include "../../str.h"
#include "../../sr_module.h"
#include <ei.h>

typedef int (*erlang_func_t)(str*, str*, ei_x_buff*, ei_x_buff*);

typedef struct erlang_binds {
	erlang_func_t do_erlang_call;
} erlang_api_t;

typedef int (*bind_erlang_f)(erlang_api_t*);

int erlang_bind(erlang_api_t *erl);

inline static int erlang_load_api(erlang_api_t *erl)
{
	bind_erlang_f bind_erlang_exports;
	if (!(bind_erlang_exports = (bind_erlang_f)find_export("erlang_bind", NO_SCRIPT, 0)))
	{
		LM_ERR("Failed to import bind_erlang\n");
		return -1;
	}
	return bind_erlang_exports(erl);
}

#endif /*ERLANG_API_H*/
