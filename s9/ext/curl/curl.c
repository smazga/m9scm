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
#include "const.h"

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
	CURLcode code;
	int idx;
	char name[] = "sys:setopt";

	code = CURLE_OK;
	idx = integer_value(name, car(x));
	handle = easylist[idx];

	if (handle == NULL)
		return error("no curl handle", x);

	option = find_magic_const(cdr(x));
	if (_curl_is_long_option(option)) {
		long parameter;
		parameter = integer_value(name, caddr(x));
		code = curl_easy_setopt(handle, option, parameter);
	}
	else if (_curl_is_string_option(option)) {
		char *parameter;
		parameter = string(caddr(x));
		code = curl_easy_setopt(handle, option, parameter);
	}

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


S9_PRIM Curl_primitives[] = {
	{ "curl:easy-init",	pp_easy_init,		0, 0, { ___,___,___ } },
	{ "curl:setopt",	pp_setopt,		3, 3, { INT,STR,___ } },
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
