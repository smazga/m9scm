static Sys_const consts[] = {
	CONST(CURLOPT_ABSTRACT_UNIX_SOCKET),
	CONST(CURLOPT_ACCEPTTIMEOUT_MS),
	CONST(CURLOPT_ACCEPT_ENCODING),
	CONST(CURLOPT_ADDRESS_SCOPE),
	CONST(CURLOPT_APPEND),
	CONST(CURLOPT_AUTOREFERER),
	CONST(CURLOPT_BUFFERSIZE),
	CONST(CURLOPT_CAINFO),
	CONST(CURLOPT_CAPATH),
	CONST(CURLOPT_CERTINFO),
	CONST(CURLOPT_CHUNK_BGN_FUNCTION),
	CONST(CURLOPT_CHUNK_DATA),
	CONST(CURLOPT_CHUNK_END_FUNCTION),
	CONST(CURLOPT_CLOSESOCKETDATA),
	CONST(CURLOPT_CLOSESOCKETFUNCTION),
	CONST(CURLOPT_CONNECTTIMEOUT),
	CONST(CURLOPT_CONNECTTIMEOUT_MS),
	CONST(CURLOPT_CONNECT_ONLY),
	CONST(CURLOPT_CONNECT_TO),
	CONST(CURLOPT_CONV_FROM_NETWORK_FUNCTION),
	CONST(CURLOPT_CONV_FROM_UTF8_FUNCTION),
	CONST(CURLOPT_CONV_TO_NETWORK_FUNCTION),
	CONST(CURLOPT_COOKIE),
	CONST(CURLOPT_COOKIEFILE),
	CONST(CURLOPT_COOKIEJAR),
	CONST(CURLOPT_COOKIELIST),
	CONST(CURLOPT_COOKIESESSION),
	CONST(CURLOPT_COPYPOSTFIELDS),
	CONST(CURLOPT_CRLF),
	CONST(CURLOPT_CRLFILE),
	CONST(CURLOPT_CUSTOMREQUEST),
	CONST(CURLOPT_DEBUGDATA),
	CONST(CURLOPT_DEBUGFUNCTION),
	CONST(CURLOPT_DEFAULT_PROTOCOL),
	CONST(CURLOPT_DIRLISTONLY),
	/* CONST(CURLOPT_DISALLOW_USERNAME_IN_URL), */
	CONST(CURLOPT_DNS_CACHE_TIMEOUT),
	CONST(CURLOPT_DNS_INTERFACE),
	CONST(CURLOPT_DNS_LOCAL_IP4),
	CONST(CURLOPT_DNS_LOCAL_IP6),
	CONST(CURLOPT_DNS_SERVERS),
	/* CONST(CURLOPT_DNS_SHUFFLE_ADDRESSES), */
	CONST(CURLOPT_DNS_USE_GLOBAL_CACHE),
	/* CONST(CURLOPT_DOH_URL), */
	CONST(CURLOPT_EGDSOCKET),
	CONST(CURLOPT_ERRORBUFFER),
	CONST(CURLOPT_EXPECT_100_TIMEOUT_MS),
	CONST(CURLOPT_FAILONERROR),
	CONST(CURLOPT_FILETIME),
	CONST(CURLOPT_FNMATCH_DATA),
	CONST(CURLOPT_FNMATCH_FUNCTION),
	CONST(CURLOPT_FOLLOWLOCATION),
	CONST(CURLOPT_FORBID_REUSE),
	CONST(CURLOPT_FRESH_CONNECT),
	CONST(CURLOPT_FTPPORT),
	CONST(CURLOPT_FTPSSLAUTH),
	CONST(CURLOPT_FTP_ACCOUNT),
	CONST(CURLOPT_FTP_ALTERNATIVE_TO_USER),
	CONST(CURLOPT_FTP_CREATE_MISSING_DIRS),
	CONST(CURLOPT_FTP_FILEMETHOD),
	CONST(CURLOPT_FTP_RESPONSE_TIMEOUT),
	CONST(CURLOPT_FTP_SKIP_PASV_IP),
	CONST(CURLOPT_FTP_SSL_CCC),
	CONST(CURLOPT_FTP_USE_EPRT),
	CONST(CURLOPT_FTP_USE_EPSV),
	CONST(CURLOPT_FTP_USE_PRET),
	CONST(CURLOPT_GSSAPI_DELEGATION),
	/* CONST(CURLOPT_HAPPY_EYEBALLS_TIMEOUT_MS), */
	/* CONST(CURLOPT_HAPROXYPROTOCOL), */
	CONST(CURLOPT_HEADER),
	CONST(CURLOPT_HEADERDATA),
	CONST(CURLOPT_HEADERFUNCTION),
	CONST(CURLOPT_HEADEROPT),
	CONST(CURLOPT_HTTP200ALIASES),
	CONST(CURLOPT_HTTPAUTH),
	CONST(CURLOPT_HTTPGET),
	CONST(CURLOPT_HTTPHEADER),
	CONST(CURLOPT_HTTPPOST),
	CONST(CURLOPT_HTTPPROXYTUNNEL),
	CONST(CURLOPT_HTTP_CONTENT_DECODING),
	CONST(CURLOPT_HTTP_TRANSFER_DECODING),
	CONST(CURLOPT_HTTP_VERSION),
	CONST(CURLOPT_IGNORE_CONTENT_LENGTH),
	CONST(CURLOPT_INFILESIZE),
	CONST(CURLOPT_INFILESIZE_LARGE),
	CONST(CURLOPT_INTERFACE),
	CONST(CURLOPT_INTERLEAVEDATA),
	CONST(CURLOPT_INTERLEAVEFUNCTION),
	CONST(CURLOPT_IOCTLDATA),
	CONST(CURLOPT_IOCTLFUNCTION),
	CONST(CURLOPT_IPRESOLVE),
	CONST(CURLOPT_ISSUERCERT),
	CONST(CURLOPT_KEEP_SENDING_ON_ERROR),
	CONST(CURLOPT_KEYPASSWD),
	CONST(CURLOPT_KRBLEVEL),
	CONST(CURLOPT_LOCALPORT),
	CONST(CURLOPT_LOCALPORTRANGE),
	CONST(CURLOPT_LOGIN_OPTIONS),
	CONST(CURLOPT_LOW_SPEED_LIMIT),
	CONST(CURLOPT_LOW_SPEED_TIME),
	CONST(CURLOPT_MAIL_AUTH),
	CONST(CURLOPT_MAIL_FROM),
	CONST(CURLOPT_MAIL_RCPT),
	CONST(CURLOPT_MAXCONNECTS),
	CONST(CURLOPT_MAXFILESIZE),
	CONST(CURLOPT_MAXFILESIZE_LARGE),
	CONST(CURLOPT_MAXREDIRS),
	CONST(CURLOPT_MAX_RECV_SPEED_LARGE),
	CONST(CURLOPT_MAX_SEND_SPEED_LARGE),
	/* CONST(CURLOPT_MIMEPOST), */
	CONST(CURLOPT_NETRC),
	CONST(CURLOPT_NETRC_FILE),
	CONST(CURLOPT_NEW_DIRECTORY_PERMS),
	CONST(CURLOPT_NEW_FILE_PERMS),
	CONST(CURLOPT_NOBODY),
	CONST(CURLOPT_NOPROGRESS),
	CONST(CURLOPT_NOPROXY),
	CONST(CURLOPT_NOSIGNAL),
	CONST(CURLOPT_OPENSOCKETDATA),
	CONST(CURLOPT_OPENSOCKETFUNCTION),
	CONST(CURLOPT_PASSWORD),
	CONST(CURLOPT_PATH_AS_IS),
	CONST(CURLOPT_PINNEDPUBLICKEY),
	CONST(CURLOPT_PIPEWAIT),
	CONST(CURLOPT_PORT),
	CONST(CURLOPT_POST),
	CONST(CURLOPT_POSTFIELDS),
	CONST(CURLOPT_POSTFIELDSIZE),
	CONST(CURLOPT_POSTFIELDSIZE_LARGE),
	CONST(CURLOPT_POSTQUOTE),
	CONST(CURLOPT_POSTREDIR),
	CONST(CURLOPT_PREQUOTE),
	CONST(CURLOPT_PRE_PROXY),
	CONST(CURLOPT_PRIVATE),
	CONST(CURLOPT_PROGRESSDATA),
	CONST(CURLOPT_PROGRESSFUNCTION),
	CONST(CURLOPT_PROTOCOLS),
	CONST(CURLOPT_PROXY),
	CONST(CURLOPT_PROXYAUTH),
	CONST(CURLOPT_PROXYHEADER),
	CONST(CURLOPT_PROXYPASSWORD),
	CONST(CURLOPT_PROXYPORT),
	CONST(CURLOPT_PROXYTYPE),
	CONST(CURLOPT_PROXYUSERNAME),
	CONST(CURLOPT_PROXYUSERPWD),
	CONST(CURLOPT_PROXY_CAINFO),
	CONST(CURLOPT_PROXY_CAPATH),
	CONST(CURLOPT_PROXY_CRLFILE),
	CONST(CURLOPT_PROXY_KEYPASSWD),
	CONST(CURLOPT_PROXY_PINNEDPUBLICKEY),
	CONST(CURLOPT_PROXY_SERVICE_NAME),
	CONST(CURLOPT_PROXY_SSLCERT),
	CONST(CURLOPT_PROXY_SSLCERTTYPE),
	CONST(CURLOPT_PROXY_SSLKEY),
	CONST(CURLOPT_PROXY_SSLKEYTYPE),
	CONST(CURLOPT_PROXY_SSLVERSION),
	CONST(CURLOPT_PROXY_SSL_CIPHER_LIST),
	CONST(CURLOPT_PROXY_SSL_OPTIONS),
	CONST(CURLOPT_PROXY_SSL_VERIFYHOST),
	CONST(CURLOPT_PROXY_SSL_VERIFYPEER),
	/* CONST(CURLOPT_PROXY_TLS13_CIPHERS), */
	CONST(CURLOPT_PROXY_TLSAUTH_PASSWORD),
	CONST(CURLOPT_PROXY_TLSAUTH_TYPE),
	CONST(CURLOPT_PROXY_TLSAUTH_USERNAME),
	CONST(CURLOPT_PROXY_TRANSFER_MODE),
	CONST(CURLOPT_PUT),
	CONST(CURLOPT_QUOTE),
	CONST(CURLOPT_RANDOM_FILE),
	CONST(CURLOPT_RANGE),
	CONST(CURLOPT_READDATA),
	CONST(CURLOPT_READFUNCTION),
	CONST(CURLOPT_REDIR_PROTOCOLS),
	CONST(CURLOPT_REFERER),
	/* CONST(CURLOPT_REQUEST_TARGET), */
	CONST(CURLOPT_RESOLVE),
	/* CONST(CURLOPT_RESOLVER_START_DATA), */
	/* CONST(CURLOPT_RESOLVER_START_FUNCTION), */
	CONST(CURLOPT_RESUME_FROM),
	CONST(CURLOPT_RESUME_FROM_LARGE),
	CONST(CURLOPT_RTSP_CLIENT_CSEQ),
	CONST(CURLOPT_RTSP_REQUEST),
	CONST(CURLOPT_RTSP_SERVER_CSEQ),
	CONST(CURLOPT_RTSP_SESSION_ID),
	CONST(CURLOPT_RTSP_STREAM_URI),
	CONST(CURLOPT_RTSP_TRANSPORT),
	CONST(CURLOPT_SASL_IR),
	CONST(CURLOPT_SEEKDATA),
	CONST(CURLOPT_SEEKFUNCTION),
	CONST(CURLOPT_SERVICE_NAME),
	CONST(CURLOPT_SHARE),
	CONST(CURLOPT_SOCKOPTDATA),
	CONST(CURLOPT_SOCKOPTFUNCTION),
	/* CONST(CURLOPT_SOCKS5_AUTH), */
	CONST(CURLOPT_SOCKS5_GSSAPI_NEC),
	CONST(CURLOPT_SOCKS5_GSSAPI_SERVICE),
	CONST(CURLOPT_SSH_AUTH_TYPES),
	/* CONST(CURLOPT_SSH_COMPRESSION), */
	CONST(CURLOPT_SSH_HOST_PUBLIC_KEY_MD5),
	CONST(CURLOPT_SSH_KEYDATA),
	CONST(CURLOPT_SSH_KEYFUNCTION),
	CONST(CURLOPT_SSH_KNOWNHOSTS),
	CONST(CURLOPT_SSH_PRIVATE_KEYFILE),
	CONST(CURLOPT_SSH_PUBLIC_KEYFILE),
	CONST(CURLOPT_SSLCERT),
	CONST(CURLOPT_SSLCERTTYPE),
	CONST(CURLOPT_SSLENGINE),
	CONST(CURLOPT_SSLENGINE_DEFAULT),
	CONST(CURLOPT_SSLKEY),
	CONST(CURLOPT_SSLKEYTYPE),
	CONST(CURLOPT_SSLVERSION),
	CONST(CURLOPT_SSL_CIPHER_LIST),
	CONST(CURLOPT_SSL_CTX_DATA),
	CONST(CURLOPT_SSL_CTX_FUNCTION),
	CONST(CURLOPT_SSL_ENABLE_ALPN),
	CONST(CURLOPT_SSL_ENABLE_NPN),
	CONST(CURLOPT_SSL_FALSESTART),
	CONST(CURLOPT_SSL_OPTIONS),
	CONST(CURLOPT_SSL_SESSIONID_CACHE),
	CONST(CURLOPT_SSL_VERIFYHOST),
	CONST(CURLOPT_SSL_VERIFYPEER),
	CONST(CURLOPT_SSL_VERIFYSTATUS),
	CONST(CURLOPT_STDERR),
	CONST(CURLOPT_STREAM_DEPENDS),
	CONST(CURLOPT_STREAM_DEPENDS_E),
	CONST(CURLOPT_STREAM_WEIGHT),
	CONST(CURLOPT_SUPPRESS_CONNECT_HEADERS),
	CONST(CURLOPT_TCP_FASTOPEN),
	CONST(CURLOPT_TCP_KEEPALIVE),
	CONST(CURLOPT_TCP_KEEPIDLE),
	CONST(CURLOPT_TCP_KEEPINTVL),
	CONST(CURLOPT_TCP_NODELAY),
	CONST(CURLOPT_TELNETOPTIONS),
	CONST(CURLOPT_TFTP_BLKSIZE),
	CONST(CURLOPT_TFTP_NO_OPTIONS),
	CONST(CURLOPT_TIMECONDITION),
	CONST(CURLOPT_TIMEOUT),
	CONST(CURLOPT_TIMEOUT_MS),
	CONST(CURLOPT_TIMEVALUE),
	/* CONST(CURLOPT_TIMEVALUE_LARGE), */
	/* CONST(CURLOPT_TLS13_CIPHERS), */
	CONST(CURLOPT_TLSAUTH_PASSWORD),
	CONST(CURLOPT_TLSAUTH_TYPE),
	CONST(CURLOPT_TLSAUTH_USERNAME),
	CONST(CURLOPT_TRANSFERTEXT),
	CONST(CURLOPT_TRANSFER_ENCODING),
	CONST(CURLOPT_UNIX_SOCKET_PATH),
	CONST(CURLOPT_UNRESTRICTED_AUTH),
	/* CONST(CURLOPT_UPKEEP_INTERVAL_MS), */
	CONST(CURLOPT_UPLOAD),
	/* CONST(CURLOPT_UPLOAD_BUFFERSIZE), */
	CONST(CURLOPT_URL),
	CONST(CURLOPT_USERAGENT),
	CONST(CURLOPT_USERNAME),
	CONST(CURLOPT_USERPWD),
	CONST(CURLOPT_USE_SSL),
	CONST(CURLOPT_VERBOSE),
	CONST(CURLOPT_WILDCARDMATCH),
	CONST(CURLOPT_WRITEDATA),
	CONST(CURLOPT_WRITEFUNCTION),
	CONST(CURLOPT_XFERINFODATA),
	CONST(CURLOPT_XFERINFOFUNCTION),
	CONST(CURLOPT_XOAUTH2_BEARER),
	CONST(CURLINFO_ACTIVESOCKET),
	CONST(CURLINFO_APPCONNECT_TIME),
	/* CONST(CURLINFO_APPCONNECT_TIME_T), */
	CONST(CURLINFO_CERTINFO),
	CONST(CURLINFO_CONDITION_UNMET),
	CONST(CURLINFO_CONNECT_TIME),
	/* CONST(CURLINFO_CONNECT_TIME_T), */
	CONST(CURLINFO_CONTENT_LENGTH_DOWNLOAD),
	/* CONST(CURLINFO_CONTENT_LENGTH_DOWNLOAD_T), */
	CONST(CURLINFO_CONTENT_LENGTH_UPLOAD),
	/* CONST(CURLINFO_CONTENT_LENGTH_UPLOAD_T), */
	CONST(CURLINFO_CONTENT_TYPE),
	CONST(CURLINFO_COOKIELIST),
	CONST(CURLINFO_EFFECTIVE_URL),
	CONST(CURLINFO_FILETIME),
	/* CONST(CURLINFO_FILETIME_T), */
	CONST(CURLINFO_FTP_ENTRY_PATH),
	CONST(CURLINFO_HEADER_SIZE),
	CONST(CURLINFO_HTTPAUTH_AVAIL),
	CONST(CURLINFO_HTTP_CONNECTCODE),
	CONST(CURLINFO_HTTP_VERSION),
	CONST(CURLINFO_LASTSOCKET),
	CONST(CURLINFO_LOCAL_IP),
	CONST(CURLINFO_LOCAL_PORT),
	CONST(CURLINFO_NAMELOOKUP_TIME),
	/* CONST(CURLINFO_NAMELOOKUP_TIME_T), */
	CONST(CURLINFO_NUM_CONNECTS),
	CONST(CURLINFO_OS_ERRNO),
	CONST(CURLINFO_PRETRANSFER_TIME),
	/* CONST(CURLINFO_PRETRANSFER_TIME_T), */
	CONST(CURLINFO_PRIMARY_IP),
	CONST(CURLINFO_PRIMARY_PORT),
	CONST(CURLINFO_PRIVATE),
	CONST(CURLINFO_PROTOCOL),
	CONST(CURLINFO_PROXYAUTH_AVAIL),
	CONST(CURLINFO_PROXY_SSL_VERIFYRESULT),
	CONST(CURLINFO_REDIRECT_COUNT),
	CONST(CURLINFO_REDIRECT_TIME),
	/* CONST(CURLINFO_REDIRECT_TIME_T), */
	CONST(CURLINFO_REDIRECT_URL),
	CONST(CURLINFO_REQUEST_SIZE),
	CONST(CURLINFO_RESPONSE_CODE),
	CONST(CURLINFO_RTSP_CLIENT_CSEQ),
	CONST(CURLINFO_RTSP_CSEQ_RECV),
	CONST(CURLINFO_RTSP_SERVER_CSEQ),
	CONST(CURLINFO_RTSP_SESSION_ID),
	CONST(CURLINFO_SCHEME),
	CONST(CURLINFO_SIZE_DOWNLOAD),
	/* CONST(CURLINFO_SIZE_DOWNLOAD_T), */
	CONST(CURLINFO_SIZE_UPLOAD),
	/* CONST(CURLINFO_SIZE_UPLOAD_T), */
	CONST(CURLINFO_SPEED_DOWNLOAD),
	/* CONST(CURLINFO_SPEED_DOWNLOAD_T), */
	CONST(CURLINFO_SPEED_UPLOAD),
	/* CONST(CURLINFO_SPEED_UPLOAD_T), */
	CONST(CURLINFO_SSL_ENGINES),
	CONST(CURLINFO_SSL_VERIFYRESULT),
	CONST(CURLINFO_STARTTRANSFER_TIME),
	/* CONST(CURLINFO_STARTTRANSFER_TIME_T), */
	CONST(CURLINFO_TLS_SESSION),
	CONST(CURLINFO_TLS_SSL_PTR),
	CONST(CURLINFO_TOTAL_TIME),
	/* CONST(CURLINFO_TOTAL_TIME_T), */
	CONST(CURLAUTH_BASIC),
	CONST(CURLAUTH_DIGEST),
	CONST(CURLAUTH_DIGEST_IE),
	/* CONST(CURLAUTH_BEARER), */
	CONST(CURLAUTH_NEGOTIATE),
	CONST(CURLAUTH_NTLM),
	CONST(CURLAUTH_NTLM_WB),
	CONST(CURLAUTH_ANY),
	CONST(CURLAUTH_ANYSAFE),
	CONST(CURLAUTH_ONLY),
	{0,0}
};
