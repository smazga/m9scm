/*
 * Scheme 9 from Empty Space, libnuklear bindings
 * By McKay Marston, 2018
 * Placed in the Public Domain
 *
 * This is a) for Plan9 only, and b) single context
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#undef string

#include <draw.h>
#include <nuklear.h>
#include "const.h"

static struct nk_context ctx;

cell pp_nk_init(cell x) {
	struct nk_user_font nkfont;
	USED(x);

	fprint(2, "initializing draw\n");

	if (initdraw(nil, nil, "s9fes") < 0)
		sysfatal("initdraw: %r");

	fprint(2, "initializing nuklear: %p\n", &ctx);

	nk_plan9_makefont(&nkfont, font);

	if (nk_init_default(&ctx, &nkfont) == 0)
		error("unable to initialize libnuklear", x);
	return UNSPECIFIC;
}

cell pp_nk_begin(cell n) {
	char *name = "nk:begin";
	char *title;
	int x, y, w, h;
	nk_flags flags;
	cell *v;
	int result;

	fprint(2, "s9_string\n");
	title = s9_string(car(n));
	fprint(2, "title: %s\n", title);

	v = vector(cadr(n));
	x = integer_value(name, v[0]);
	y = integer_value(name, v[1]);
	w = integer_value(name, v[2]);
	h = integer_value(name, v[3]);

	flags = integer_value(name, caddr(n));

	fprint(2, "calling nk_begin %p %d %d %d %d %x\n", &ctx, x, y, w, h, flags);
	result = nk_begin(&ctx, title, nk_rect(x, y, w, h), flags);
	fprint(2, "begun: %d\n", result);

	return (result > 0)? TRUE: FALSE;
}

cell pp_nk_end(cell x) {
	USED(x);
	nk_end(&ctx);
	return UNSPECIFIC;
}

cell pp_nk_clear(cell x) {
	USED(x);
	nk_clear(&ctx);
	return UNSPECIFIC;
}

cell pp_nk_free(cell x) {
	USED(x);

	nk_free(&ctx);
	return UNSPECIFIC;
}

cell pp_nk_layout_row_dynamic(cell x) {
	char *name = "nk:layout-row-dynamic";
	int height, cols;

	fprint(2, "layout_row_dynamic\n");
	height = integer_value(name, car(x));
	cols = integer_value(name, cadr(x));

	nk_layout_row_dynamic(&ctx, (float)height, cols);
	return UNSPECIFIC;
}

cell pp_nk_button_label(cell x) {
	/* char *name = "nk:button-label"; */
	char *str;

	str = s9_string(car(x));
	nk_button_label(&ctx, str);
	return UNSPECIFIC;
}

cell pp_nk_render(cell x) {
	USED(x);

	fprint(2, "render\n");
	draw(screen, screen->r, display->black, nil, ZP);
	nk_plan9_render(&ctx, screen);
	flushimage(display, 1);
	return UNSPECIFIC;
}

S9_PRIM Nk_primitives[] = {
	{ "nk:init",               pp_nk_init,               0, 0, { ___,___,___ } },
	{ "nk:begin",              pp_nk_begin,              3, 3, { STR,VEC,INT } },
	{ "nk:end",                pp_nk_end,                0, 0, { ___,___,___ } },
	{ "nk:clear",              pp_nk_clear,              0, 0, { ___,___,___ } },
	{ "nk:free",               pp_nk_free,               0, 0, { ___,___,___ } },
	{ "nk:layout-row-dynamic", pp_nk_layout_row_dynamic, 2, 2, { INT,INT,___ } },
	{ "nk:button-label",       pp_nk_button_label,       1, 1, { STR,___,___ } },
	{ "nk:load-consts",        pp_nk_load_consts,        0, 0, { ___,___,___ } },
	{ "nk:render",             pp_nk_render,             0, 0, { ___,___,___ } },
	{ NULL }
};

void nk_ext_init(void) {
	add_primitives("nk", Nk_primitives);
}
