#' Get a SnupySession object
#'
#'\code{SnupySession} generates a S6 class
#'
#'@usage
#' SnupySession$new(file.path = NULL)
#'@param filepath a path to a .r file that contains the config
#'@details SnupySession objects are used to establish a connection to a SNuPy server through HTTP(S).
#' An important feature of session is their cache a \code{\link{SnupyMemory}} object.
#' The HTTP authentification and proxy password are encrypted asymetrically, so that storing a session object through save(...) will not expose the password.
#'\describe{
#'	\item{URL}{SNUPY url}
#'	\item{USER}{snupy user}
#'	\item{PASS}{optional password for USER. If null the password is asked for interactively (recommended)}
#'	\item{KEYFILE}{optional location of a .pem file used to encrypt PASS and PROXYPASS. See Details.}
#'	\item{CACHEDIR}{path to cache dir (/tmp)}
#'	\item{PROXY}{proxy host}
#'	\item{PROXYPORT}{proxy port}
#'	\item{PROXYUSER}{proxy user}
#'	\item{PROXYPASS}{Proxy password}
#'	\item{PROXYAUTH}{Curl proxy authentifcation method}
#'	\item{COMPRESSION}{logical value if compression should be used for HTTP transfers}
#'	\item{DEBUG}{logical value if debug information should be displayed.}
#'	\item{SHOWSQL}{logical value if SQL commands should be printed in debug mode}
#'	\item{SHOWPROGRESS}{logical value if progress on SnupyCollection methods should be displayed (experimental).}
#'}
#'
#'@section Methods:
#'These methods are accessible:
#'
#'\describe{
#' \item{\code{load.records()}}{Caches all \code{\link{SnupyData}} objects.}
#' \item{\code{cache(FUN, ...)}}{cache the call to FUN(...)}
#' \item{\code{get_proxy_password()}}{returns proxy password in plain text, use with caution.}
#' \item{\code{get_password()}}{returns password in plain text, use with caution.}
#' \item{\code{reset_password()}}{resets the current password and more importantly re-encrypts it with the current key. Usefull when transfering session objects between machines.}
#'}
#'@section Class-Methods:
#'These methods are accessible:
#'
#'\describe{
#' \item{\code{SnupySession$encrypt(txt, pubkey = my_pubkey())}}{Is used to encrypt short texts (n<245), such as passwords}
#' \item{\code{SnupySession$decrypt(secret, key = my_key())}}{Used to decrypt texts.}
#' \item{\code{SnupySession$encrypt.object(obj, file.path = NULL, pubkey = my_pubkey())}}{Encrypts the serialized output of an object applying RSA cypher. The RSA key is encrypted with AES and stored alongside. If file.path is not NULL the result is stored in a file and the path to the file is returned.}
#' \item{\code{SnupySession$decrypt.object(enc.obj = NULL, file.path = NULL, key = my_key())}}{Decrypts cyphered objects.}
#'}
#'@return Object of R6 class which hold connection information
#'@importFrom R6 R6Class
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{SnupyMemory}}, \code{\link{snupy.query}}, \code{\link{snupy.request}}
#'@examples
#' ################
#' # snupy_config.R
#' ################
#' URL = "http://localhost:3000"
#' USER   = NULL
#' PASS   = NULL
#' KEYFILE   = NULL
#' CACHEDIR   = "~/tmp"
#' PROXY = NULL
#' PROXYPORT = NULL
#' PROXYUSER = NULL
#' PROXYPASS = NULL
#' PROXYAUTH = NULL
#' COMPRESSION = FALSE
#' DEBUG	 = TRUE
#' SHOWPROGRESS = TRUE
#' SHOWSQL  = FALSE
#' ################
#' # EOF ##########
#' ################
#' ssion <- SnupySession$new('snupy_config.R')
#' snupy.query("SHOW TABLES", ssion)
#'@aliases session
#'@family session
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
SnupySession <- R6Class("SnupySession",
										 public = list(
										 	URL = NULL,
										 	COMPRESSION = NULL,
										 	DEBUG = NULL,
										 	SHOWSQL = NULL,
										 	SHOWPROGRESS = FALSE,
										 	USER = NULL,
										 	PASS = NULL,
										 	CACHEDIR = NULL,
										 	PROXY = NULL,
										 	PROXYPORT = NULL,
										 	PROXYUSER = NULL,
										 	PROXYPASS = NULL,
										 	PROXYAUTH = NULL,
										 	CACHE = NULL,
										 	KEYFILE = NULL,
										 	initialize = function(filepath, cache.name = NULL) {
										 		source(filepath, local = TRUE)
										 		self$CACHEDIR   = sprintf("%s/.libRSnupy", gsub("^~", Sys.getenv("HOME"), x=CACHEDIR)) ## replace ~ as HOME
										 		self$URL      = URL
										 		self$USER   = USER
										 		self$PASS   = PASS
										 		self$PROXY      = PROXY
										 		self$PROXYPORT      = PROXYPORT
										 		self$PROXYUSER      = PROXYUSER
										 		self$PROXYPASS      = PROXYPASS
										 		self$PROXYAUTH      = PROXYAUTH
										 		self$KEYFILE = ifelse(exists("KEYFILE"), KEYFILE, private$get_keyfile())
										 		self$COMPRESSION      = COMPRESSION
										 		self$DEBUG    = DEBUG
										 		self$SHOWSQL  = SHOWSQL
										 		self$SHOWPROGRESS = ifelse(exists("SHOWPROGRESS"), SHOWPROGRESS, FALSE)
										 		#Sys.setenv("USER_KEY" = self$KEYFILE)
										 		#Sys.setenv("USER_PUBKEY" = sprintf("%s.pub", self$KEYFILE))
										 		if (!is.null(CACHEDIR)){
										 			if(.Platform$OS.type == "unix") {
										 				if (!file.exists(self$CACHEDIR)) mkdirs(self$CACHEDIR)
										 			} else {
										 				if (!file.exists(self$CACHEDIR)) dir.create(self$CACHEDIR, recursive = T)
										 			}
										 			cache.path = `if`(is.null(cache.name),
										 													NULL,
										 													sprintf("%s/%s", self$CACHEDIR, cache.name))
										 			self$CACHE = SnupyMemory$new(cache.path)
										 		} else {
										 			self$CACHEDIR = NULL
										 			self$CACHE = NULL
										 		}
										 		if (!is.null(self$PASS)){
													warning("Storing the password in a plain textfile is generally not a good idea. You will be asked for PASS if it is not in the config.")
										 		}
										 		private$set_password()
										 		self
										 	},
										 	get_password = function(){
										 		invisible(private$decrypt(self$PASS))
										 	},
										 	get_proxy_password = function(){
										 		invisible(private$decrypt(self$PROXYPASS))
										 	},
										 	get_key = function(){
										 		private$.key
										 	},
										 	get_pubkey = function(){
										 		private$.pubkey
										 	},
										 	load.records = function(){
										 		cat("Loading entity_groups...\n")
										 		EntityGroup$all(session = self)
										 		cat("Loading entities...\n")
										 		Entity$all(session = self)
										 		cat("Loading specimen...\n")
										 		SpecimenProbe$all(session = self)
										 		cat("Loading samples...\n")
										 		Sample$all(session = self)
										 		cat("Loading vcf files...\n")
										 		VcfFile$all(session = self)
										 		cat("Loading tags...\n")
										 		SnupyTag$all(session = self)
										 		cat("Done\n")
												return(TRUE)
										 	},
										 	cache = function(FUN, ...){
										 		if (missing(FUN))
										 			return(self$CACHE)
										 		if (!is.null(self$CACHE)){
										 			self$CACHE$cache(FUN, ...)
										 		} else {
										 			FUN(...)
										 		}
										 	},
										 	reset_password = function(){
										 		self$PASS = NULL
										 		private$set_password()
										 	},
										 	reset_cache = function(fresh = FALSE){
										 		cache.path = NULL
										 		if (!is.null(self$CACHE)){
											 		if (!fresh)
											 			cache.path = self$CACHE$cache.path
											 		self$CACHE = NULL
											 		self$CACHE = SnupyMemory$new(cache.path)
										 		} else {
													if (fresh){
														self$CACHE = SnupyMemory$new(NULL)
													}
										 		}
										 	},
										 	print = function(...) {
										 		cat(sprintf("<%s>\n", class(self)[[1]]))
										 		cat(sprintf("\t URL: %s\n", self$URL))
										 		cat(sprintf("\t USER: %s\n", self$USER))
										 		cat(sprintf("\t PASS: %s\n", gsub(".", "*", self$get_password())))
										 		cat(sprintf("\t KEY: %s\n", private$.key))
										 		cat(sprintf("\t PUBKEY: %s\n", private$.pubkey))
										 		cat(sprintf("\t CACHEDIR: %s\n", self$CACHEDIR))
										 		cat(sprintf("\t PROXY: %s\n", self$PROXY))
										 		cat(sprintf("\t PROXYPORT: %s\n", self$PROXYPORT))
										 		cat(sprintf("\t PROXYUSER: %s\n", self$PROXYUSER))
										 		cat(sprintf("\t PROXYPASS: %s\n", gsub(".", "*", self$get_proxy_password())))
										 		cat(sprintf("\t PROXYAUTH: %s\n", self$PROXYAUTH))
										 		cat(sprintf("\t COMPRESSION: %s\n", self$COMPRESSION))
										 		cat(sprintf("\t DEBUG: %s\n", self$DEBUG))
										 		cat(sprintf("\t SHOWSQL: %s\n", self$SHOWSQL))
										 		cat(sprintf("\t SHOWPROGRESS: %s\n", self$SHOWPROGRESS))
										 		cat(sprintf("\t CACHE SIZE: %s\n", ifelse(!is.null(self$CACHE), self$CACHE$size(), "-1")))
										 		invisible(self)
										 	},
										 	finalize = function() {
										 		self$CACHE = NULL
										 	}
										 ),
										 private = list(
										 	.key = NULL,
										 	.pubkey = NULL,
										 	set_password = function(){
										 		if (!is.null(self$USER)){
										 			if (is.null(self$PASS)){
										 				self$PASS = getPass(sprintf("Please enter the password for %s@%s", self$USER, self$URL)) %>% private$encrypt(.)
										 			} else {
										 				self$PASS = private$encrypt(self$PASS)
										 			}
										 			# make a test
										 			result = snupy.query("SHOW TABLES", session = self, cache.use = FALSE)
										 			if (is.null(result)){
										 				warning("Session can't establish a connection.")
										 			}
										 		} else {
										 			self$PASS = NULL
										 			message("Not using authentication. Set USER in conf to use it.")
										 		}
										 	},
										 	get_keyfile = function(){
										 		keyfile = Sys.getenv("USER_KEY")
										 		pubkeyfile = Sys.getenv("USER_PUBKEY")
										 		if (keyfile != ""){
										 			cat("Global USER_KEY found in your environment.")
										 		} else {
										 			usrname = self$USER
										 			if (is.null(usrname))
										 				usrname = ""
										 			keyfile = file.path(self$CACHEDIR, sprintf("%s@%s_rsa", usrname, self$URL %>% digest(., "crc32")))
										 			pubkeyfile = sprintf("%s.pub", keyfile)
										 		}
										 		private$.key = keyfile
										 		private$.pubkey = pubkeyfile

												if (!file.exists(keyfile)){
													cat(sprintf("Public/Private keypair not found at %s.\nGenerating one for you...", keyfile))
													key = rsa_keygen()
													write_pem(key, path = keyfile)
													write_pem(key$pubkey, path = pubkeyfile)
													rm(key)
													cat(sprintf("DONE.\n%s", keyfile))
												} else {
													cat(sprintf("Public/Private keypair found at %s", keyfile))
													# test if the file is a pem file
													tmp = read_pem(keyfile)
													stopifnot(!is.null(tmp[["PRIVATE KEY"]]))
												}
												keyfile
										 	},
										 	encrypt = function(txt){
										 		SnupySession$encrypt(txt, private$.pubkey)
										 	},
										 	decrypt = function(secret){
										 		SnupySession$decrypt(secret, private$.key)
										 	}
										 )
)
SnupySession$encrypt = function(txt, pubkey = my_pubkey()){
	if (is.null(txt))
		return(NULL)
	if (nchar(txt) > 245 ){
		stop("Cannot encrypt anything larger than 245 characters.")
	}
	if (pubkey %>% is.null){
		stop("No public key is available. Make sure my_pubkey() works.")
	}
	charToRaw(txt) %>% rsa_encrypt(., pubkey = pubkey) %>% return
}
SnupySession$decrypt = function(secret, key = my_key()){
	if (is.null(secret))
		return(NULL)
	if (key %>% is.null){
		stop("No private key is available. Make sure my_key() works.")
	}
	rsa_decrypt(secret, key = key, password = askpass) %>% rawToChar(.) %>% return
}

#' @export
SnupySession$encrypt.object = function(obj, file.path = NULL, pubkey = my_pubkey()){
	result = list(
		pass = NULL,
		size = NULL,
		obj = NULL,
		key = pubkey
	)
	# create a secret token
	pass = openssl::rand_bytes(32)
	aes = AES(key = pass)
	# encrypt the pass with RSA and make it the first element in the result
	result[["pass"]] = pass %>% rsa_encrypt(., pubkey = pubkey)
	# serialize the object and use AES to encrypt it with pass
	obj.ser = serialize(obj, NULL)
	result$size = obj.ser %>% length
	obj.ser.missing = 16 - result$size %% 16
	obj.ser = c(obj.ser, as.raw(rep(0L, times = obj.ser.missing)))
	result$obj = aes$encrypt(text = obj.ser)
	if (!is.null(file.path)){
		fout = file(description = file.path, "wb")
		serialize(result, fout)
		close(fout)
		return(file.path)
	}
	result
}

#' @export
SnupySession$decrypt.object = function(enc.obj = NULL, file.path = NULL, key = my_key()){
	if (enc.obj %>% missing){
		fin = file(description = file.path, "rb")
		enc.obj = unserialize(fin)
		close(fin)
	}
	# decrypt the pass
	pass = enc.obj$pass %>% rsa_decrypt(., key = key, password = askpass)
	aes = AES(key = pass)
	return(
		enc.obj$obj %>% aes$decrypt(., raw=T) %>% extract(., 1:enc.obj$size) %>% unserialize(.)
	)
}
