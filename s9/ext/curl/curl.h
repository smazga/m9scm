#include "const.h"

cell pp_curl_load_consts(cell x) {
	int i;

	for(i = 0; consts[i].name != NULL; i++) {
		char cmd[128];
		sprintf(cmd, "(define curl:%s %d)", consts[i].name, consts[i].value);
		eval(xsread(cmd));
	}

	return UNSPECIFIC;
}


/* This is taken directly from the curl source...I guess this is one way to handle dynamic types in c */
#define _curl_is_long_option(option)                                          \
  (0 < (option) && (option) < CURLOPTTYPE_OBJECTPOINT)

#define _curl_is_string_option(option)                                        \
  ((option) == CURLOPT_ABSTRACT_UNIX_SOCKET ||                                \
   (option) == CURLOPT_ACCEPT_ENCODING ||                                     \
   (option) == CURLOPT_CAINFO ||                                              \
   (option) == CURLOPT_CAPATH ||                                              \
   (option) == CURLOPT_COOKIE ||                                              \
   (option) == CURLOPT_COOKIEFILE ||                                          \
   (option) == CURLOPT_COOKIEJAR ||                                           \
   (option) == CURLOPT_COOKIELIST ||                                          \
   (option) == CURLOPT_CRLFILE ||                                             \
   (option) == CURLOPT_CUSTOMREQUEST ||                                       \
   (option) == CURLOPT_DEFAULT_PROTOCOL ||                                    \
   (option) == CURLOPT_DNS_INTERFACE ||                                       \
   (option) == CURLOPT_DNS_LOCAL_IP4 ||                                       \
   (option) == CURLOPT_DNS_LOCAL_IP6 ||                                       \
   (option) == CURLOPT_DNS_SERVERS ||                                         \
   (option) == CURLOPT_EGDSOCKET ||                                           \
   (option) == CURLOPT_FTPPORT ||                                             \
   (option) == CURLOPT_FTP_ACCOUNT ||                                         \
   (option) == CURLOPT_FTP_ALTERNATIVE_TO_USER ||                             \
   (option) == CURLOPT_INTERFACE ||                                           \
   (option) == CURLOPT_ISSUERCERT ||                                          \
   (option) == CURLOPT_KEYPASSWD ||                                           \
   (option) == CURLOPT_KRBLEVEL ||                                            \
   (option) == CURLOPT_LOGIN_OPTIONS ||                                       \
   (option) == CURLOPT_MAIL_AUTH ||                                           \
   (option) == CURLOPT_MAIL_FROM ||                                           \
   (option) == CURLOPT_NETRC_FILE ||                                          \
   (option) == CURLOPT_NOPROXY ||                                             \
   (option) == CURLOPT_PASSWORD ||                                            \
   (option) == CURLOPT_PINNEDPUBLICKEY ||                                     \
   (option) == CURLOPT_PRE_PROXY ||                                           \
   (option) == CURLOPT_PROXY ||                                               \
   (option) == CURLOPT_PROXYPASSWORD ||                                       \
   (option) == CURLOPT_PROXYUSERNAME ||                                       \
   (option) == CURLOPT_PROXYUSERPWD ||                                        \
   (option) == CURLOPT_PROXY_CAINFO ||                                        \
   (option) == CURLOPT_PROXY_CAPATH ||                                        \
   (option) == CURLOPT_PROXY_CRLFILE ||                                       \
   (option) == CURLOPT_PROXY_KEYPASSWD ||                                     \
   (option) == CURLOPT_PROXY_PINNEDPUBLICKEY ||                               \
   (option) == CURLOPT_PROXY_SERVICE_NAME ||                                  \
   (option) == CURLOPT_PROXY_SSLCERT ||                                       \
   (option) == CURLOPT_PROXY_SSLCERTTYPE ||                                   \
   (option) == CURLOPT_PROXY_SSLKEY ||                                        \
   (option) == CURLOPT_PROXY_SSLKEYTYPE ||                                    \
   (option) == CURLOPT_PROXY_SSL_CIPHER_LIST ||                               \
   (option) == CURLOPT_PROXY_TLSAUTH_PASSWORD ||                              \
   (option) == CURLOPT_PROXY_TLSAUTH_USERNAME ||                              \
   (option) == CURLOPT_PROXY_TLSAUTH_TYPE ||                                  \
   (option) == CURLOPT_RANDOM_FILE ||                                         \
   (option) == CURLOPT_RANGE ||                                               \
   (option) == CURLOPT_REFERER ||                                             \
   (option) == CURLOPT_RTSP_SESSION_ID ||                                     \
   (option) == CURLOPT_RTSP_STREAM_URI ||                                     \
   (option) == CURLOPT_RTSP_TRANSPORT ||                                      \
   (option) == CURLOPT_SERVICE_NAME ||                                        \
   (option) == CURLOPT_SOCKS5_GSSAPI_SERVICE ||                               \
   (option) == CURLOPT_SSH_HOST_PUBLIC_KEY_MD5 ||                             \
   (option) == CURLOPT_SSH_KNOWNHOSTS ||                                      \
   (option) == CURLOPT_SSH_PRIVATE_KEYFILE ||                                 \
   (option) == CURLOPT_SSH_PUBLIC_KEYFILE ||                                  \
   (option) == CURLOPT_SSLCERT ||                                             \
   (option) == CURLOPT_SSLCERTTYPE ||                                         \
   (option) == CURLOPT_SSLENGINE ||                                           \
   (option) == CURLOPT_SSLKEY ||                                              \
   (option) == CURLOPT_SSLKEYTYPE ||                                          \
   (option) == CURLOPT_SSL_CIPHER_LIST ||                                     \
   (option) == CURLOPT_TLSAUTH_PASSWORD ||                                    \
   (option) == CURLOPT_TLSAUTH_TYPE ||                                        \
   (option) == CURLOPT_TLSAUTH_USERNAME ||                                    \
   (option) == CURLOPT_UNIX_SOCKET_PATH ||                                    \
   (option) == CURLOPT_URL ||                                                 \
   (option) == CURLOPT_USERAGENT ||                                           \
   (option) == CURLOPT_USERNAME ||                                            \
   (option) == CURLOPT_USERPWD ||                                             \
   (option) == CURLOPT_XOAUTH2_BEARER ||                                      \
   0)

/* evaluates to true if info expects a pointer to char * argument */
#define _curl_is_string_info(info)                                            \
  (CURLINFO_STRING < (info) && (info) < CURLINFO_LONG)

/* evaluates to true if info expects a pointer to long argument */
#define _curl_is_long_info(info)                                              \
  (CURLINFO_LONG < (info) && (info) < CURLINFO_DOUBLE)

/* evaluates to true if info expects a pointer to double argument */
#define _curl_is_double_info(info)                                            \
  (CURLINFO_DOUBLE < (info) && (info) < CURLINFO_SLIST)

/* true if info expects a pointer to struct curl_slist * argument */
#define _curl_is_slist_info(info)                                       \
  (((info) == CURLINFO_SSL_ENGINES) || ((info) == CURLINFO_COOKIELIST))

/* evaluates to true if option takes a POST data argument (void* or char*) */
#define _curl_is_postfields_option(option)                                    \
  ((option) == CURLOPT_POSTFIELDS ||                                          \
   (option) == CURLOPT_COPYPOSTFIELDS ||                                      \
   0)
