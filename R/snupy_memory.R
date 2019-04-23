#' Get a SnupyMemory object
#'
#'\code{SnupyMemory} generates a S6 class
#'
#'@usage
#' SnupyMemory$new(cache.path = NULL, verbose = FALSE)
#'@param cache.path path to file that is used to store the cache if requested.
#'@param verbose logical value
#'
#'@details SnupyMemory provides methods to cache the result of functions calls.
#'
#'@section Methods:
#'These methods are accessible:
#'
#'\describe{
#' \item{\code{store(file = NULL)}}{Saves cached values to file}
#' \item{\code{load(file = NULL)}}{loads cached values from file }
#' \item{\code{reset()}}{resets cache and performs gc(reset=T)}
#' \item{\code{show()}}{Shows list of cache elements}
#' \item{\code{size()}}{Shows list of cache elements}
#' \item{\code{call()}}{Shows current cache size.}
#' \item{\code{add(key, value)}}{add key/value pair to cache}
#' \item{\code{get_or_add(key, value)}}{sets key/value pair or returs value of key, if already set.}
#' \item{\code{get(key)}}{returns key}
#' \item{\code{get_key(FUN, ..., memoize.name = NULL, memoize.ignore.parameter = c())}}{
#' Returns a key for a call to FUN(...), if memoize.name is set, it return that value.
#' memoize.ignore.parameter can be used to excluse parameters from ...
#' (such as time stamps or session ids), which change during calls, but don't affect the result.}
#' \item{\code{cache(FUN, ..., memoize.force = FALSE, memoize.name = NULL, memoize.ignore.parameter = c())}}{
#' Adds an entry to the cache. CACHE[get_key(FUN, ..., memoize.name = NULL, memoize.ignore.parameter = c())] = FUN(...)
#' if it was not computed before. If it was the cache result is returned.
#' \cr
#' If memoize.force = TRUE the computation is forced.
#' }
#'}
#'@return Object of R6 class which hold connection information
#'@importFrom R6 R6Class
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{SnupySession}}, \code{\link{snupy.query}}, \code{\link{snupy.request}}
#'@examples
#' > library(snupy)
#' > mem = SnupyMemory$new(verbose = T)
#' > system.time(mem$cache(Sys.sleep, 2))
#' Computing new value for FUN.db8e490a925a60e62212cefc7674ca02
#' Returning value for FUN.db8e490a925a60e62212cefc7674ca02
#' User      System elapsed
#' 0.00        0.00        2.03
#' > system.time(mem$cache(Sys.sleep, 2))
#' Returning value for FUN.db8e490a925a60e62212cefc7674ca02
#' User      System elapsed
#' 0.03        0.00        0.03
#'@aliases session
#'@family cache
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
SnupyMemory <- R6Class("SnupyMemory",
										 public = list(
										 	cache.path = NULL,
										 	verbose = FALSE,
										 	initialize = function(cache.file = NULL, verbose = FALSE){
										 		if (!(is.null(cache.file)) && !is.null(self$cache.path) && file.exists(self$cache.path)){
										 			self$cache.path = cache.path
										 			self$load(cache.path)
										 		}
										 		self$verbose = verbose
										 	},
										 	finalize = function(){
										 		private$cache.store = NULL
										 	},
										 	store = function(file = NULL){
										 		if (is.null(file)){
										 			file = self$cache.path
										 		}
										 		stopifnot(!is.null(file))
										 		storage = list(store = private$cache.store, processed = private$cache.processed)
										 		save(storage, file = file)
										 	},
										 	load = function(file = NULL){
										 		if (is.null(file))
										 			file = self$cache.path
										 		stopifnot(!is.null(file))
										 		if (file.exists(file)){
										 			cat("loading cache: ", file, "\n", sep="")
										 			self$reset()
										 			load(file)
										 			private$cache.store = storage$store
										 			private$cache.processed = storage$processed
										 			rm(storage)
										 		} else {
										 			warning(sprintf("%s does not exist", file))
										 		}
										 	},
										 	reset = function(value = NULL){
										 		if (value %>% is.null){
											 		private$cache.store = list()
											 		private$cache.processed = list()
										 		} else {
										 			private$cache.store[[value]] = NULL
										 			private$cache.processed[[value]] = NULL
										 		}
										 		gc(reset=T)
										 	},
										 	show = function(){
										 		private$cache.store
										 	},
										 	num.elements = function(){
										 		length(private$cache.store)
										 	},
										 	size = function(){
										 		object.size(private$cache.store) %>% format(units="auto")
										 	},
										 	call = function(FUN, ..., memoize.force = FALSE, memoize.name = NULL, memoize.ignore.parameter = c()){
										 		self$cache(FUN, ..., memoize.force = memoize.force, memoize.name = memoize.name, memoize.ignore.parameter = memoize.ignore.parameter)
										 	},
										 	add = function(key, value){
										 		private$cache.store[[key]] = value
										 		private$cache.processed[[key]] = TRUE
										 	},
										 	get_or_add = function(key, value){
												if (self$get(key) %>% is.null)
													add(key, value)
										 		self$get(key)
										 	},
										 	get = function(key){
										 		private$cache.store[[key]]
										 	},
										 	get_key = function(FUN, ..., memoize.name = NULL, memoize.ignore.parameter = c()){
										 		if (is.null(memoize.name) || (is.logical(memoize.name))){
										 			params = list(...) %>% unlist
										 			msk = sapply(params, simplify=T, function(x){
										 				inherits(x, "SnupySession") || inherits(x, "SnupyMemory")
										 			})
										 			params = params[!msk]
										 			params = params[!params %in% memoize.ignore.parameter]
										 			paramsname = digest(params)
										 		} else {
										 			# paramsname = memoize.name

										 			return(memoize.name)
										 		}
										 		if (!inherits(FUN, "fseq")){
										 			if (is.function(FUN)){
										 				i = 1
										 				if (match.call()[[1]] == "self$get_key")
										 					i = 2
										 				funname = paste0(as.character(substitute(FUN, parent.frame(i) )), collapse="-")
										 			} else {
										 				funname = as.character(FUN)
										 			}
										 		} else {
										 			# funname = sprintf("magrittr-%s", digest(as.character(substitute(FUN, parent.frame() ))))
										 			funname = sprintf("magrittr-%s", digest(functions(FUN)))
										 		}
										 		key = sprintf("%s.%s", as.character(funname), as.character(paramsname))
										 		key
										 	},
											cache = function(FUN, ..., memoize.force = FALSE, memoize.name = NULL, memoize.ignore.parameter = c()){
												key = self$get_key(FUN=FUN, ..., memoize.name = memoize.name, memoize.ignore.parameter = memoize.ignore.parameter)
												if (is.function(FUN)){
													if (is.null(private$cache.processed[[key]]) || memoize.force){
														if (self$verbose)
															cat(sprintf("Computing new value for %s\n", key))
														result = FUN(...)
														self$add(key, result)
													}
												} else {
													if (is.null(private$cache.processed[[key]]) || memoize.force){
														if (self$verbose)
															cat(sprintf("Computing new value for %s\n", key))
														self$add(key, FUN)
													}
												}
												if (self$verbose)
													cat(sprintf("Returning value for %s\n", key))
												invisible(self$get(key))
											}
										 ),
										 private = list(
										 	cache.store = list(),
										 	cache.processed = list()
										 )
)
