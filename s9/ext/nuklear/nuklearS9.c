/*
 * Scheme 9 from Empty Space, libnuklear bindings
 * By McKay Marston, 2018
 * Placed in the Public Domain
 *
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_IMPLEMENTATION
#define NK_INCLUDE_FONT_BAKING
#define NK_INCLUDE_DEFAULT_FONT
#ifdef unix
 #include "upstream/nuklear.h"
#else
 #ifdef plan9
  #include "nuklear_plan9.h"
 #endif
#endif

#include "const.h"

static struct nk_context global_ctx;
static struct nk_font_atlas global_atlas;

cell pp_nk_init(cell x) {
	struct nk_font *font;
	const void* img;
	int img_width, img_height;
	nk_handle handle;

	USED(x);

	handle.ptr = NULL;
	handle.id = 0;

	nk_font_atlas_init_default(&global_atlas);
	nk_font_atlas_begin(&global_atlas);
	font = nk_font_atlas_add_default(&global_atlas, 13, NULL);
	img = nk_font_atlas_bake(&global_atlas, &img_width, &img_height, NK_FONT_ATLAS_RGBA32);
	nk_font_atlas_end(&global_atlas, handle, NULL);

	if (nk_init_default(&global_ctx, &font->handle) == 0)
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

	title = string(car(n));

	v = vector(cadr(n));
	x = integer_value(name, v[0]);
	y = integer_value(name, v[1]);
	w = integer_value(name, v[2]);
	h = integer_value(name, v[3]);

	flags = integer_value(name, caddr(n));

	result = nk_begin(&global_ctx, title, nk_rect(x, y, w, h), flags);

	return (result > 0)? TRUE: FALSE;
}

cell pp_nk_end(cell x) {
	USED(x);
	nk_end(&global_ctx);
	return UNSPECIFIC;
}

cell pp_nk_clear(cell x) {
	USED(x);
	nk_clear(&global_ctx);
	return UNSPECIFIC;
}

cell pp_nk_free(cell x) {
	USED(x);

	nk_font_atlas_clear(&global_atlas);
	nk_free(&global_ctx);
	return UNSPECIFIC;
}

cell pp_nk_layout_row_dynamic(cell x) {
	char *name = "nk:layout-row-dynamic";
	int height, cols;

	height = integer_value(name, car(x));
	cols = integer_value(name, cadr(x));

	nk_layout_row_dynamic(&global_ctx, (float)height, cols);
	return UNSPECIFIC;
}

cell pp_nk_button_label(cell x) {
	char *name = "nk:button-label";
	char *str;

	str = string(car(x));
	nk_button_label(&global_ctx, str);
	return UNSPECIFIC;
}

S9_PRIM Nk_primitives[] = {
	{ "nk:init",			pp_nk_init,			0, 0, { ___,___,___ } },
	{ "nk:begin",			pp_nk_begin,			3, 3, { STR,VEC,INT } },
	{ "nk:end",			pp_nk_end,			0, 0, { ___,___,___ } },
	{ "nk:clear",			pp_nk_clear,			0, 0, { ___,___,___ } },
	{ "nk:free",			pp_nk_free,			0, 0, { ___,___,___ } },
	{ "nk:layout-row-dynamic",	pp_nk_layout_row_dynamic,	2, 2, { INT,INT,___ } },
	{ "nk:button-label",		pp_nk_button_label,		1, 1, { STR,___,___ } },
	{ "nk:load-consts",		pp_nk_load_consts,		0, 0, { ___,___,___ } },
	{ NULL }
};

void nk_ext_init(void) {
	add_primitives("nk", Nk_primitives);
}
