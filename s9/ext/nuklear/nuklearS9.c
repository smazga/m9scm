/*
 * Scheme 9 from Empty Space, libnuklear bindings
 * By McKay Marston, 2018
 * Placed in the Public Domain
 *
 * There's a single context used. There will probably never be a complex enough use case to do anything else.
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#define NK_INCLUDE_DEFAULT_ALLOCATOR
#define NK_IMPLEMENTATION
#ifdef unix
 #include "upstream/nuklear.h"
#else
 #ifdef plan9
  #include "nuklear_plan9.h"
 #endif
#endif

static struct nk_context global_ctx;
static struct nk_user_font global_font;

cell pp_nk_init(cell x) {
	struct nk_context *ctx = &global_ctx;
	struct nk_user_font *font = &global_font;
	USED(x);

	if (nk_init_default(ctx, font) == 0)
		error("unable to initialize libnuklear", x);
	return UNSPECIFIC;
}

cell pp_nk_clear(cell x) {
	USED(x);
	nk_clear(global_ctx);
	return UNSPECIFIC;
}

cell pp_nk_free(cell x) {
	USED(x);
	nk_free(global_ctx);
	return UNSPECIFIC;
}

S9_PRIM Nk_primitives[] = {
	{ "nk:init",	pp_nk_init,	0, 0, { ___,___,___ } },
	{ "nk:clear",	pp_nk_clear,	0, 0, { ___,___,___ } },
	{ "nk:free",	pp_nk_free,	0, 0, { ___,___,___ } },
	{ NULL }
};

void nk_ext_init(void) {
	add_primitives("nk", Nk_primitives);
}
