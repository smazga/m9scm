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
#include "s9-ffi.h"

#undef tag
#undef narg
#include <fcall.h>
#include <thread.h>
#include <9p.h>

#ifdef BROKEN
cell pp_sys_postmountsrv(void) {
	cell srv;
	char *sname, *mtpt;
	int flag = 0;
	char name[] = "sys:postmountsrv";

	srv = parg(1);
	sname = string(parg(2));
	mtpt = string(parg(3));
	/* flag = integer_value(name, cadddr(x)); */

	postmountsrv(srv, sname, mtpt, flag);
	return TRUE;
}
#endif

extern int sys_convS2M(cell, uchar*, int);
cell pp_9p_write(void) {
	int fd = integer_value("9p:write", parg(1));
	cell x = parg(2);
	uchar buf[8192+IOHDRSZ];
	int len = sys_convS2M(x, buf, sizeof buf);
	int r;

	if (len < 0)
		return FALSE;

	r = write(fd, buf, len);
	return make_long_integer(r);
}

S9_PRIM ninep_primitives[] = {
/* {"sys:postmountsrv",	pp_sys_postmountsrv,	3, 4, { SYM,STR,INT } }, */
 {"9p:write",			pp_9p_write,			2, 2, { INT,VEC,___ } },
 { NULL }
};

void ninep_init(void) {
	add_primitives("plan9-9p", ninep_primitives);
}
