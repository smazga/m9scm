/* ********************************************************* */
/* This is the wrong place for this. unix/get_magic_value should be replaced with this */
#define	K(x)	{#x, (int)x}
struct Magic_const {
	char*	name;
	int	value;
};

typedef struct Magic_const Magic_const;
static Magic_const magic_const[] = {
	K(CURLOPT_ABSTRACT_UNIX_SOCKET),
	K(CURLOPT_ACCEPTTIMEOUT_MS),
	K(CURLOPT_ACCEPT_ENCODING),
	K(CURLOPT_ADDRESS_SCOPE),
	K(CURLOPT_APPEND),
	K(CURLOPT_AUTOREFERER),
	K(CURLOPT_BUFFERSIZE),
	K(CURLOPT_CAINFO),
	K(CURLOPT_CAPATH),
	K(CURLOPT_CERTINFO),
	K(CURLOPT_CHUNK_BGN_FUNCTION),
	K(CURLOPT_CHUNK_DATA),
	K(CURLOPT_CHUNK_END_FUNCTION),
	K(CURLOPT_CLOSESOCKETDATA),
	K(CURLOPT_CLOSESOCKETFUNCTION),
	K(CURLOPT_CONNECTTIMEOUT),
	K(CURLOPT_CONNECTTIMEOUT_MS),
	K(CURLOPT_CONNECT_ONLY),
	K(CURLOPT_CONNECT_TO),
	K(CURLOPT_CONV_FROM_NETWORK_FUNCTION),
	K(CURLOPT_CONV_FROM_UTF8_FUNCTION),
	K(CURLOPT_CONV_TO_NETWORK_FUNCTION),
	K(CURLOPT_COOKIE),
	K(CURLOPT_COOKIEFILE),
	K(CURLOPT_COOKIEJAR),
	K(CURLOPT_COOKIELIST),
	K(CURLOPT_COOKIESESSION),
	K(CURLOPT_COPYPOSTFIELDS),
	K(CURLOPT_CRLF),
	K(CURLOPT_CRLFILE),
	K(CURLOPT_CUSTOMREQUEST),
	K(CURLOPT_DEBUGDATA),
	K(CURLOPT_DEBUGFUNCTION),
	K(CURLOPT_DEFAULT_PROTOCOL),
	K(CURLOPT_DIRLISTONLY),
	/* K(CURLOPT_DISALLOW_USERNAME_IN_URL), */
	K(CURLOPT_DNS_CACHE_TIMEOUT),
	K(CURLOPT_DNS_INTERFACE),
	K(CURLOPT_DNS_LOCAL_IP4),
	K(CURLOPT_DNS_LOCAL_IP6),
	K(CURLOPT_DNS_SERVERS),
	/* K(CURLOPT_DNS_SHUFFLE_ADDRESSES), */
	K(CURLOPT_DNS_USE_GLOBAL_CACHE),
	/* K(CURLOPT_DOH_URL), */
	K(CURLOPT_EGDSOCKET),
	K(CURLOPT_ERRORBUFFER),
	K(CURLOPT_EXPECT_100_TIMEOUT_MS),
	K(CURLOPT_FAILONERROR),
	K(CURLOPT_FILETIME),
	K(CURLOPT_FNMATCH_DATA),
	K(CURLOPT_FNMATCH_FUNCTION),
	K(CURLOPT_FOLLOWLOCATION),
	K(CURLOPT_FORBID_REUSE),
	K(CURLOPT_FRESH_CONNECT),
	K(CURLOPT_FTPPORT),
	K(CURLOPT_FTPSSLAUTH),
	K(CURLOPT_FTP_ACCOUNT),
	K(CURLOPT_FTP_ALTERNATIVE_TO_USER),
	K(CURLOPT_FTP_CREATE_MISSING_DIRS),
	K(CURLOPT_FTP_FILEMETHOD),
	K(CURLOPT_FTP_RESPONSE_TIMEOUT),
	K(CURLOPT_FTP_SKIP_PASV_IP),
	K(CURLOPT_FTP_SSL_CCC),
	K(CURLOPT_FTP_USE_EPRT),
	K(CURLOPT_FTP_USE_EPSV),
	K(CURLOPT_FTP_USE_PRET),
	K(CURLOPT_GSSAPI_DELEGATION),
	/* K(CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS), */
	/* K(CURLOPT_HAPROXYPROTOCOL), */
	K(CURLOPT_HEADER),
	K(CURLOPT_HEADERDATA),
	K(CURLOPT_HEADERFUNCTION),
	K(CURLOPT_HEADEROPT),
	K(CURLOPT_HTTP200ALIASES),
	K(CURLOPT_HTTPAUTH),
	K(CURLOPT_HTTPGET),
	K(CURLOPT_HTTPHEADER),
	K(CURLOPT_HTTPPOST),
	K(CURLOPT_HTTPPROXYTUNNEL),
	K(CURLOPT_HTTP_CONTENT_DECODING),
	K(CURLOPT_HTTP_TRANSFER_DECODING),
	K(CURLOPT_HTTP_VERSION),
	K(CURLOPT_IGNORE_CONTENT_LENGTH),
	K(CURLOPT_INFILESIZE),
	K(CURLOPT_INFILESIZE_LARGE),
	K(CURLOPT_INTERFACE),
	K(CURLOPT_INTERLEAVEDATA),
	K(CURLOPT_INTERLEAVEFUNCTION),
	K(CURLOPT_IOCTLDATA),
	K(CURLOPT_IOCTLFUNCTION),
	K(CURLOPT_IPRESOLVE),
	K(CURLOPT_ISSUERCERT),
	K(CURLOPT_KEEP_SENDING_ON_ERROR),
	K(CURLOPT_KEYPASSWD),
	K(CURLOPT_KRBLEVEL),
	K(CURLOPT_LOCALPORT),
	K(CURLOPT_LOCALPORTRANGE),
	K(CURLOPT_LOGIN_OPTIONS),
	K(CURLOPT_LOW_SPEED_LIMIT),
	K(CURLOPT_LOW_SPEED_TIME),
	K(CURLOPT_MAIL_AUTH),
	K(CURLOPT_MAIL_FROM),
	K(CURLOPT_MAIL_RCPT),
	K(CURLOPT_MAXCONNECTS),
	K(CURLOPT_MAXFILESIZE),
	K(CURLOPT_MAXFILESIZE_LARGE),
	K(CURLOPT_MAXREDIRS),
	K(CURLOPT_MAX_RECV_SPEED_LARGE),
	K(CURLOPT_MAX_SEND_SPEED_LARGE),
	/* K(CURLOPT_MIMEPOST), */
	K(CURLOPT_NETRC),
	K(CURLOPT_NETRC_FILE),
	K(CURLOPT_NEW_DIRECTORY_PERMS),
	K(CURLOPT_NEW_FILE_PERMS),
	K(CURLOPT_NOBODY),
	K(CURLOPT_NOPROGRESS),
	K(CURLOPT_NOPROXY),
	K(CURLOPT_NOSIGNAL),
	K(CURLOPT_OPENSOCKETDATA),
	K(CURLOPT_OPENSOCKETFUNCTION),
	K(CURLOPT_PASSWORD),
	K(CURLOPT_PATH_AS_IS),
	K(CURLOPT_PINNEDPUBLICKEY),
	K(CURLOPT_PIPEWAIT),
	K(CURLOPT_PORT),
	K(CURLOPT_POST),
	K(CURLOPT_POSTFIELDS),
	K(CURLOPT_POSTFIELDSIZE),
	K(CURLOPT_POSTFIELDSIZE_LARGE),
	K(CURLOPT_POSTQUOTE),
	K(CURLOPT_POSTREDIR),
	K(CURLOPT_PREQUOTE),
	K(CURLOPT_PRE_PROXY),
	K(CURLOPT_PRIVATE),
	K(CURLOPT_PROGRESSDATA),
	K(CURLOPT_PROGRESSFUNCTION),
	K(CURLOPT_PROTOCOLS),
	K(CURLOPT_PROXY),
	K(CURLOPT_PROXYAUTH),
	K(CURLOPT_PROXYHEADER),
	K(CURLOPT_PROXYPASSWORD),
	K(CURLOPT_PROXYPORT),
	K(CURLOPT_PROXYTYPE),
	K(CURLOPT_PROXYUSERNAME),
	K(CURLOPT_PROXYUSERPWD),
	K(CURLOPT_PROXY_CAINFO),
	K(CURLOPT_PROXY_CAPATH),
	K(CURLOPT_PROXY_CRLFILE),
	K(CURLOPT_PROXY_KEYPASSWD),
	K(CURLOPT_PROXY_PINNEDPUBLICKEY),
	K(CURLOPT_PROXY_SERVICE_NAME),
	K(CURLOPT_PROXY_SSLCERT),
	K(CURLOPT_PROXY_SSLCERTTYPE),
	K(CURLOPT_PROXY_SSLKEY),
	K(CURLOPT_PROXY_SSLKEYTYPE),
	K(CURLOPT_PROXY_SSLVERSION),
	K(CURLOPT_PROXY_SSL_CIPHER_LIST),
	K(CURLOPT_PROXY_SSL_OPTIONS),
	K(CURLOPT_PROXY_SSL_VERIFYHOST),
	K(CURLOPT_PROXY_SSL_VERIFYPEER),
	/* K(CURLOPT_PROXY_TLS13_CIPHERS), */
	K(CURLOPT_PROXY_TLSAUTH_PASSWORD),
	K(CURLOPT_PROXY_TLSAUTH_TYPE),
	K(CURLOPT_PROXY_TLSAUTH_USERNAME),
	K(CURLOPT_PROXY_TRANSFER_MODE),
	K(CURLOPT_PUT),
	K(CURLOPT_QUOTE),
	K(CURLOPT_RANDOM_FILE),
	K(CURLOPT_RANGE),
	K(CURLOPT_READDATA),
	K(CURLOPT_READFUNCTION),
	K(CURLOPT_REDIR_PROTOCOLS),
	K(CURLOPT_REFERER),
	/* K(CURLOPT_REQUEST_TARGET), */
	K(CURLOPT_RESOLVE),
	/* K(CURLOPT_RESOLVER_START_DATA), */
	/* K(CURLOPT_RESOLVER_START_FUNCTION), */
	K(CURLOPT_RESUME_FROM),
	K(CURLOPT_RESUME_FROM_LARGE),
	K(CURLOPT_RTSP_CLIENT_CSEQ),
	K(CURLOPT_RTSP_REQUEST),
	K(CURLOPT_RTSP_SERVER_CSEQ),
	K(CURLOPT_RTSP_SESSION_ID),
	K(CURLOPT_RTSP_STREAM_URI),
	K(CURLOPT_RTSP_TRANSPORT),
	K(CURLOPT_SASL_IR),
	K(CURLOPT_SEEKDATA),
	K(CURLOPT_SEEKFUNCTION),
	K(CURLOPT_SERVICE_NAME),
	K(CURLOPT_SHARE),
	K(CURLOPT_SOCKOPTDATA),
	K(CURLOPT_SOCKOPTFUNCTION),
	/* K(CURLOPT_SOCKS5_AUTH), */
	K(CURLOPT_SOCKS5_GSSAPI_NEC),
	K(CURLOPT_SOCKS5_GSSAPI_SERVICE),
	K(CURLOPT_SSH_AUTH_TYPES),
	/* K(CURLOPT_SSH_COMPRESSION), */
	K(CURLOPT_SSH_HOST_PUBLIC_KEY_MD5),
	K(CURLOPT_SSH_KEYDATA),
	K(CURLOPT_SSH_KEYFUNCTION),
	K(CURLOPT_SSH_KNOWNHOSTS),
	K(CURLOPT_SSH_PRIVATE_KEYFILE),
	K(CURLOPT_SSH_PUBLIC_KEYFILE),
	K(CURLOPT_SSLCERT),
	K(CURLOPT_SSLCERTTYPE),
	K(CURLOPT_SSLENGINE),
	K(CURLOPT_SSLENGINE_DEFAULT),
	K(CURLOPT_SSLKEY),
	K(CURLOPT_SSLKEYTYPE),
	K(CURLOPT_SSLVERSION),
	K(CURLOPT_SSL_CIPHER_LIST),
	K(CURLOPT_SSL_CTX_DATA),
	K(CURLOPT_SSL_CTX_FUNCTION),
	K(CURLOPT_SSL_ENABLE_ALPN),
	K(CURLOPT_SSL_ENABLE_NPN),
	K(CURLOPT_SSL_FALSESTART),
	K(CURLOPT_SSL_OPTIONS),
	K(CURLOPT_SSL_SESSIONID_CACHE),
	K(CURLOPT_SSL_VERIFYHOST),
	K(CURLOPT_SSL_VERIFYPEER),
	K(CURLOPT_SSL_VERIFYSTATUS),
	K(CURLOPT_STDERR),
	K(CURLOPT_STREAM_DEPENDS),
	K(CURLOPT_STREAM_DEPENDS_E),
	K(CURLOPT_STREAM_WEIGHT),
	K(CURLOPT_SUPPRESS_CONNECT_HEADERS),
	K(CURLOPT_TCP_FASTOPEN),
	K(CURLOPT_TCP_KEEPALIVE),
	K(CURLOPT_TCP_KEEPIDLE),
	K(CURLOPT_TCP_KEEPINTVL),
	K(CURLOPT_TCP_NODELAY),
	K(CURLOPT_TELNETOPTIONS),
	K(CURLOPT_TFTP_BLKSIZE),
	K(CURLOPT_TFTP_NO_OPTIONS),
	K(CURLOPT_TIMECONDITION),
	K(CURLOPT_TIMEOUT),
	K(CURLOPT_TIMEOUT_MS),
	K(CURLOPT_TIMEVALUE),
	/* K(CURLOPT_TIMEVALUE_LARGE), */
	/* K(CURLOPT_TLS13_CIPHERS), */
	K(CURLOPT_TLSAUTH_PASSWORD),
	K(CURLOPT_TLSAUTH_TYPE),
	K(CURLOPT_TLSAUTH_USERNAME),
	K(CURLOPT_TRANSFERTEXT),
	K(CURLOPT_TRANSFER_ENCODING),
	K(CURLOPT_UNIX_SOCKET_PATH),
	K(CURLOPT_UNRESTRICTED_AUTH),
	/* K(CURLOPT_UPKEEP_INTERVAL_MS), */
	K(CURLOPT_UPLOAD),
	/* K(CURLOPT_UPLOAD_BUFFERSIZE), */
	K(CURLOPT_URL),
	K(CURLOPT_USERAGENT),
	K(CURLOPT_USERNAME),
	K(CURLOPT_USERPWD),
	K(CURLOPT_USE_SSL),
	K(CURLOPT_VERBOSE),
	K(CURLOPT_WILDCARDMATCH),
	K(CURLOPT_WRITEDATA),
	K(CURLOPT_WRITEFUNCTION),
	K(CURLOPT_XFERINFODATA),
	K(CURLOPT_XFERINFOFUNCTION),
	K(CURLOPT_XOAUTH2_BEARER),
	{0,0}
};

int find_magic_const(cell x) {
	char*		name = string(car(x));
	Magic_const	*k;

	for (k = magic_const; k->name; k++)
		if (strcmp(k->name, name) == 0)
			return k->value;
	return -1;
}

cell pp_sys_magic_const(cell x) {
	return make_integer(find_magic_const(x));
}
/* ********************************************************* */

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
