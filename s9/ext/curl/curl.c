/*
 * Scheme 9 from Empty Space, libcurl easy_interface
 * By McKay Marston, 2018
 * Placed in the Public Domain
 *
 * Hooks into the libcurl easy API
 */


/* REMOVE ME:
 *  - make sure cleanup is called
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#include <curl/curl.h>

static CURL *easylist[1024];
static int easyidx = 0;
char error_buffer[CURL_ERROR_SIZE];

cell pp_easy_init(cell x) {
	CURL *curl;

	if (easyidx >= sizeof(easylist))
		easyidx = 0;

	curl = easylist[easyidx];
	if (curl != NULL) {
		curl_easy_cleanup(curl);
		easylist[easyidx] = NULL;
	}

	curl = curl_easy_init();
	if (curl == NULL)
		return error("curl:make-handle", x);

	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, error_buffer);

	fprintf(stderr, "easyidx: %d\n", easyidx);
	easylist[easyidx] = curl;
	return make_integer(easyidx++);
}

cell pp_cleanup(cell x) {
	CURL *handle;
	int idx;

	idx = integer_value("curl:cleanup", car(x));
	handle = easylist[idx];

	if (handle != NULL) {
		curl_easy_cleanup(handle);
		easylist[idx] = NULL;
	}

	return UNSPECIFIC;
}

cell pp_setopt(cell x) {
	CURL *handle;
	CURLoption option;
	char *parameter;
	CURLcode code;
	int idx;
	char name[] = "sys:setopt";

	idx = integer_value(name, car(x));
	handle = easylist[idx];

	if (handle == NULL)
		return error("no curl handle", x);

	option = integer_value(name, cadr(x));
	parameter = string(caddr(x));

	code = curl_easy_setopt(handle, option, parameter);
	if (code != CURLE_OK) {
		char err[16+CURL_ERROR_SIZE];
		memset(err, 0, sizeof(err));
		snprintf(err, sizeof(err)-1, "%s: %s", name, curl_easy_strerror(code));
		return error(err, x);
	}

	return UNSPECIFIC;
}

cell pp_perform(cell x) {
	CURL *handle;
	CURLcode code;
	int idx;
	char name[] = "sys:perform";

	idx = integer_value(name, car(x));
	handle = easylist[idx];

	code = curl_easy_perform(handle);
	if (code != CURLE_OK) {
		char err[16+CURL_ERROR_SIZE];
		memset(err, 0, sizeof(err));
		snprintf(err, sizeof(err)-1, "%s: %s", name, curl_easy_strerror(code));
		return error(err, x);
	}

	return UNSPECIFIC;
}

/* ********************************************************* */
/* This is the wrong place for this. unix/get_magic_value should be replaced with this */
#define	K(x)	{#x, (int)x}
struct Magic_const {
	char*	name;
	int	value;
};

typedef struct Magic_const Magic_const;
static Magic_const magic_const[] = {
	K(CURLOPT_URL),
	{0,0}
};
cell pp_sys_magic_const(cell x) {
	char*		name = string(car(x));
	Magic_const	*k;

	for (k = magic_const; k->name; k++)
		if (strcmp(k->name, name) == 0)
			return make_integer(k->value);
	return FALSE;
}
/* ********************************************************* */

S9_PRIM Curl_primitives[] = {
	{ "curl:easy-init",	pp_easy_init,		0, 0, { ___,___,___ } },
	{ "curl:setopt",	pp_setopt,		3, 3, { ___,INT,STR } },
	{ "curl:perform",	pp_perform,		1, 1, { INT,___,___ } },
	{ "curl:cleanup",	pp_cleanup,		1, 1, { INT,___,___ } },
	{ "sys:magic-const",	pp_sys_magic_const,	1, 1, { STR,___,___ } },
};

void curl_init(void) {
	curl_global_init(CURL_GLOBAL_ALL);
	memset(easylist, 0, sizeof(easylist));
	memset(error_buffer, 0, sizeof(error_buffer));
	add_primitives("curl", Curl_primitives);
}
