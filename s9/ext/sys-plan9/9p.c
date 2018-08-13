/*
 * Scheme 9 from Empty Space, 9p Bindings
 * By McKay Marston, 2018
 * Placed in the Public Domain
 *
 * 9p bindings for creating Plan9 style filesystems in scheme.
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#undef tag
#include <fcall.h>
#include <thread.h>
#include <9p.h>

cell pp_9p_fsreader(cell x) {
};

cell pp_sys_postmountsrv(cell x ) {
	cell srv;
	char *sname, *mtpt;
	int flag;
	char name[] = "sys:postmountsrv";

	srv = car(x);
	sname = string(cadr(x));
	mtpt = string(caddr(x));
	flag = integer_value(name, cadddr(x));

	postmountsrv(srv, sname, mtpt, flag);
	return TRUE;
}

S9_PRIM ninep_primitives[] = {
 {"sys:postmountsrv",	pp_sys_postmountsrv,	3, 4, { SYM,STR,INT } },
};

void ninep_init(void) {
	add_primitives("sys-plan9", ninep_primitives);
}
