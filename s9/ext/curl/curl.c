/*
 * Scheme 9 from Empty Space, libcurl easy_interface
 * By McKay Marston, 2018
 * Placed in the Public Domain
 *
 * Hooks into the libcurl easy API
 */

#include "s9core.h"
#include "s9import.h"
#include "s9ext.h"

#include <curl/curl.h>
#include "curl.h"

#define MAX_SESSIONS	1024
#define BUFFER_SIZE	8192

/* total hack */
#define SESSION sessionlist[integer_value(name, car(x))]

#define CHECK_SESSION(s, c) \
	if (s->handle == NULL) \
		return error("no curl handle", c);

#define CHECK_CODE(c) \
	if (c != CURLE_OK) { \
		char err[16+CURL_ERROR_SIZE]; \
		memset(err, 0, sizeof(err)); \
		snprintf(err, sizeof(err)-1, "%s: %s", name, curl_easy_strerror(code)); \
		return error(err, x); \
	}

struct Session {
	CURL *handle;
	char *buffer;
	size_t bufferlen;
	struct curl_slist *cookies;
};

typedef struct Session Session;
static Session *sessionlist[MAX_SESSIONS];
static int sessionidx = 0;
char error_buffer[CURL_ERROR_SIZE];

cell slist2cell(struct curl_slist *slist) {
	cell n, new;
	struct curl_slist *each;
	int count;

	if (slist == NULL)
		return make_vector(0);

	each = slist;
	count = 0;

	/* ghetto, but we need a count for making a vector */
	while (each) {
		count++;
		each = each->next;
	}

	n = make_vector(count);
	count = 0;
	each = slist;

	save(n); /* why? - based on sys_convM2D */
	while (each) {
		new = make_string(each->data, strlen(each->data));
		vector(n)[count++] = new;
		each = each->next;
	}
	unsave(1);
	curl_slist_free_all(each);

	return n;
}

cell pp_easy_init(cell x) {
	CURL *curl;

	if (sessionidx >= sizeof(sessionlist))
		sessionidx = 0;

	curl = sessionlist[sessionidx];
	if (curl != NULL) {
		curl_easy_cleanup(curl);
		sessionlist[sessionidx] = NULL;
	}

	curl = curl_easy_init();
	if (curl == NULL)
		return error("curl:make-handle", x);
	curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, error_buffer);

	sessionlist[sessionidx] = calloc(1, sizeof(Session));
	sessionlist[sessionidx]->handle = curl;

	return make_integer(sessionidx++);
}

cell pp_cleanup(cell x) {
	Session *session;
	char *name = "curl:cleanup";

	session = SESSION;

	if (session->cookies != NULL)
		curl_slist_free_all(session->cookies);

	if (session->handle != NULL) {
		curl_easy_cleanup(session->handle);
		free(session->buffer);
		SESSION = NULL;
	}

	return UNSPECIFIC;
}

cell pp_setopt(cell x) {
	Session *session;
	CURLoption option;
	CURLcode code;
	char name[] = "sys:setopt";

	code = CURLE_OK;
	session = SESSION;
	CHECK_SESSION(session, x);

	option = find_magic_const(cdr(x));
	fprintf(stderr, "option: %d: ", option);
	if (_curl_is_long_option(option)) {
		long parameter;
		parameter = integer_value(name, caddr(x));
		fprintf(stderr, "  %ld\n", parameter);
		code = curl_easy_setopt(session->handle, option, parameter);
	}
	else if (_curl_is_string_option(option)) {
		char *parameter;
		parameter = string(caddr(x));
		fprintf(stderr, "  %s\n", parameter);
		code = curl_easy_setopt(session->handle, option, parameter);
	}

	if (code != CURLE_OK) {
		char err[16+CURL_ERROR_SIZE];
		memset(err, 0, sizeof(err));
		snprintf(err, sizeof(err)-1, "%s: %s", name, curl_easy_strerror(code));
		return error(err, x);
	}

	return UNSPECIFIC;
}

cell pp_getinfo(cell x) {
	Session *session;
	CURLINFO info;
	CURLcode code = CURLE_OK;
	char *name = "curl:getinfo";

	session = SESSION;
	CHECK_SESSION(session, x);

	info = find_magic_const(cdr(x));

	if (_curl_is_long_info(info)) {
		long result;
		code = curl_easy_getinfo(session->handle, info, &result);
		CHECK_CODE(code);
		return make_integer(result);
	}

	if (_curl_is_string_info(info)) {
		char *result;
		code = curl_easy_getinfo(session->handle, info, &result);
		CHECK_CODE(code);
		return make_string(result, strlen(result));
	}

	if (_curl_is_slist_info(info)) {
		cell n;
		struct curl_slist *result = NULL;

		code = curl_easy_getinfo(session->handle, info, &result);
		CHECK_CODE(code);

		n = slist2cell(result);
		curl_slist_free_all(result);

		return n;
	}

	return NIL;
}

static size_t write_callback(void *ptr, size_t size, size_t nmemb, void *data) {
	Session *session;
	size_t realsize;

	realsize = size * nmemb;
	session = (Session*)data;
	session->buffer = realloc(session->buffer, session->bufferlen + realsize + 1);

	if (session->buffer) {
		memcpy(&(session->buffer[session->bufferlen]), ptr, realsize);
		session->bufferlen += realsize;
		session->buffer[session->bufferlen] = 0;
	}

	return realsize;
}

cell pp_perform(cell x) {
	Session *session;
	CURLcode code;
	char name[] = "sys:perform";
	cell n;

	session = SESSION;
	CHECK_SESSION(session, x);

	curl_easy_setopt(session->handle, CURLOPT_WRITEFUNCTION, write_callback);
	curl_easy_setopt(session->handle, CURLOPT_WRITEDATA, (void *)session);
	curl_easy_setopt(session->handle, CURLOPT_COOKIEFILE, "");

	code = curl_easy_perform(session->handle);
	if (code != CURLE_OK) {
		char err[16+CURL_ERROR_SIZE];
		memset(err, 0, sizeof(err));
		snprintf(err, sizeof(err)-1, "%s: %s", name, curl_easy_strerror(code));
		return error(err, x);
	}
	curl_easy_getinfo(session->handle, CURLINFO_COOKIELIST, &(session->cookies));

	n = make_string("", session->bufferlen);
	memcpy(string(n), (char *)session->buffer, session->bufferlen);
	return n;
}

cell pp_get_cookies(cell x) {
	Session *session;
	char *name = "curl:get-cookies";

	session = SESSION;
	CHECK_SESSION(session, x);

	return slist2cell(session->cookies);
}

cell pp_set_cookies(cell x) {
	Session *session;
	CURLcode code;
	char *name = "curl:set-cookies";
	int count, i;
	cell *v;

	session = SESSION;
	CHECK_SESSION(session, x);

	if (!vector_p(cadr(x)))
		return error("argument #2 not a vector", x);

	v = vector(cadr(x));
	count = vector_len(cadr(x));

	fprintf(stderr, "cookie count: %d\n", count);
	for (i = 0; i < count; i++) {
		char *cookie = string(v[i]);
		code = curl_easy_setopt(session->handle, CURLOPT_COOKIELIST, cookie);
		CHECK_CODE(code);
	}

	return NIL;
}

S9_PRIM Curl_primitives[] = {
	{ "curl:easy-init",	pp_easy_init,		0, 0, { ___,___,___ } },
	{ "curl:setopt",	pp_setopt,		3, 3, { INT,STR,___ } },
	{ "curl:perform",	pp_perform,		1, 1, { INT,___,___ } },
	{ "curl:cleanup",	pp_cleanup,		1, 1, { INT,___,___ } },
	{ "curl:getinfo",	pp_getinfo,		2, 2, { INT,STR,___ } },
	{ "curl:get-cookies",   pp_get_cookies,         1, 1, { INT,___,___ } },
	{ "curl:set-cookies",   pp_set_cookies,         2, 2, { INT,VEC,___ } },
	{ "sys:magic-const",	pp_sys_magic_const,	1, 1, { STR,___,___ } },
};

void curl_init(void) {
	curl_global_init(CURL_GLOBAL_ALL);
	memset(sessionlist, 0, sizeof(sessionlist));
	memset(error_buffer, 0, sizeof(error_buffer));
	add_primitives("curl", Curl_primitives);
}
