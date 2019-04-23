#' Get a SnupyDataCollection object
#'
#'\code{SnupyDataCollection} generates a S6 class
#'
#'@usage
#' SnupyDataCollection$new(cache.path = NULL, verbose = FALSE)
#'@param cache.path path to file that is used to store the cache if requested.
#'@param verbose logical value
#'
#'@details SnupyDataCollection provides methods to cache the result of functions calls.
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
#'@importFrom Matrix colSums rowSums colMeans rowMeans diag t crossprod
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{SnupySession}}, \code{\link{snupy.query}}, \code{\link{snupy.request}}
#'@examples
#' > library(snupy)
#' > mem = SnupyDataCollection$new(verbose = T)
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
SnupyDataCollection <- R6Class("SnupyDataCollection",
										 public = list(
										 	klass = NULL,
										 	ids = NULL,
										 	session = NULL,
										 	collection = list(),
										 	#initialize = function(...) {
										 	initialize = function(ids, session, klass, objects = NULL){
										 		self$build(ids = ids, session = session, klass = klass, objects = objects)
										 	},
										 	build = function(ids, session, klass, objects = NULL){
										 		self$session <- session
										 		self$ids <- unique(ids)
										 		self$klass = klass
										 		self$refresh(objects)
										 	},
										 	refresh = function(new_collection = NULL){
										 		if (is.null(new_collection)){
											 		if (is.null(self$ids)){
											 			self$collection = list()
											 		} else {
											 			templates = self$klass$all(session = self$session, ids = self$ids)
											 			self$collection = sapply(1:nrow(templates), simplify = F, function(i){
											 				self$klass$new(id = templates[i, ]$id, session = self$session, template = templates[i,])
											 			})
											 		}
										 		} else {
										 			self$collection = new_collection %>% unlist
										 			self$ids = sapply(new_collection, simplify=F, function(e){e$id})
										 		}
										 		self
										 	},
										 	fields = function(name){
										 		sapply(self$collection, simplify=F, function(x){
										 			x$field(name)
										 		})
										 	},
										 	field = function(name){
										 		self$fields(name)
										 	},
										 	filter = function(FUN_OR_VEC){
												ret = self$clone()
												if (FUN_OR_VEC %>% is.function){
													FUN_OR_VEC = sapply(ret$collection, simplify = T, FUN = FUN_OR_VEC) %>% as.vector
													if (length(FUN_OR_VEC) != length(ret$collection)){
														warning("Trying to select more elements than present in colleciton.")
													}
												}
												if (inherits(FUN_OR_VEC, "SnupyDataCollection")){
													FUN_OR_VEC = which(self$ids %in% FUN_OR_VEC$ids)
												}
												if (length(FUN_OR_VEC) > 0){
													ret$collection = ret$collection[FUN_OR_VEC]
												} else {
													ret$collection = list()
												}
												ret$ids = sapply(ret$collection, simplify=F, function(x){x$id}) %>% unlist
												invisible(ret)
										 	},
										 	select = function(ids = NULL, names = NULL){
												ids.mask = rep(T, times = self$size)
												names.mask = rep(T, times = self$size)
												if (!is.null(ids))
													ids.mask = (self$fields("id") %>% unlist) %in% ids
												if (!is.null(names))
													names.mask = grepl(paste0(names, collapse="|"), (self$fields("name") %>% unlist))

												msk = ids.mask & names.mask
												self$filter(msk)
										 	},
										 	which = function(FUN){
										 		sapply(self$collection, simplify = T, function(e){
										 			ret = FUN(e)
										 			if (is.null(ret)){
										 				ret = FALSE
										 			} else if (is.na(ret)){
										 				ret = FALSE
										 			}
										 				ret = FALSE
										 			ret
										 		}) %>% which
										 	},
										 	each = function(method.name, ..., as.collection = T, use.ids = T){
										 		show_progress = F
										 		if (self$session$SHOWPROGRESS){
										 			show_progress = T
										 			self$session$SHOWPROGRESS = F
										 			i = 0
										 			if (is.character(method.name)){
										 				cat(sprintf("Executing %s on %d elements\n", method.name, self$size))
										 			}
										 			pb = txtProgressBar(0, self$size, style=3)
										 		}
										 		result = sapply(self$collection, simplify=F, function(x){
										 			if (is.function(method.name)){
										 				ret = method.name(x, ...)
										 			} else {
											 			ret = x[[method.name]]
											 			if (is.function(ret))
											 				ret = ret(...)
										 			}
										 			if (show_progress){
										 				i <<- i + 1
										 				setTxtProgressBar(pb, i)
										 			}
										 			ret
										 		})
										 		if (use.ids)
										 			names(result) = self$ids

										 		if (as.collection && all(sapply(result, function(x){inherits(x, "SnupyData") || inherits(x, "SnupyTag")}))){
										 			result = SnupyDataCollection$new(NULL, session = self$session, SnupyDataCollection, objects = result)
										 		}
										 		if (show_progress){
										 			close(pb)
										 			self$session$SHOWPROGRESS = T
										 		}
										 		#names(result) = as.character(self$fields("id"))
										 		result
										 	},
										 	includes = function(object){
										 		any(self$collection == object)
										 	},
										 	includes.all = function(object){
										 		all(object %in% self$collection)
										 	},
											tags = function(tag_level = c("self", "entity_groups", "entities", "specimen_probes", "samples", "vcf_files")){
												self$each("tags", tag_level, as.collection = T)
												#if (tag_level[[1]] == "self"){
												#	SnupyTag$find.by.object(self, self$session)
												#} else {
												#	self[[tag_level[[1]]]]()$each("tags") %>% Reduce("+", .)
												#}
											},
											has_tag = function(tagvalue = NULL, tag_level = c("self", "entity_groups", "entities", "specimen_probes", "samples", "vcf_files")){
												#self$tags(tag_level)$each("include", tagvalue)
												self$each("has_tag", tagvalue, tag_level) %>% unlist
											},
											variation.matrix.quick = function(attribute = c("present", "baf"), min.dp = 100, col.row.name = NULL, cache.use = F, transform.fun = NULL, ...){
												sample_ids = self$each("samples") %>% sapply(., function(s){s$id}) %>% unlist
												cols = c("sample_id", "variation_id")
												if (attribute[[1]] == "baf")
													cols = c(cols, "alt_reads/(alt_reads+ref_reads) AS baf")
												tbl = buildStatement(
													table = "variation_calls",
													columns = cols,
													dp = list(raw=sprintf("dp >= %d", min.dp)),
													sample_id = sample_ids
												) %>% snupy.query(., session = self$session, cache.use = cache.use)
												if (!is.null(transform.fun))
													tbl = transform.fun(tbl)
												if (attribute[[1]] == "baf"){
													mat = sparseMatrix(i = tbl$sample_id, j=tbl$variation_id, x=tbl$baf)
												} else {
													mat = sparseMatrix(i = tbl$sample_id, j=tbl$variation_id)
												}
												colnames(mat) = 1:ncol(mat) %>% as.character
												rownames(mat) = 1:nrow(mat) %>% as.character
												mat = mat[rownames(mat) %in% (tbl$sample_id %>% as.character %>% unique), colSums(mat) > 1]
												rm(tbl)
												# now we should try to map the sample id attribute to a proper thing, such as sample name
												if (!is.null(col.row.name) && col.row.name != "id"){
													message("col.row.name is ignored for now")
												}
												mat
											},
											variation.matrix.transform.symbol = function(tbl, modelname = "Vep::Ensembl"){
												Aqua$annotations(session = self$session,
																				 variation_ids = unique(tbl$variation_id),
																				 organism_id = self$organism_id,
																				 modelname = modelname,
																				 attributes = c("variation_id", "ensembl_gene_id")
																				 )
											},
											variation.matrix = function(attribute = c("present", "baf"), min.dp = 100, col.row.name = "id", ...){
												if (attribute[[1]] == "present"){
													vars = self$each("variations", dp = list(raw=sprintf("dp >= %d", min.dp)), ..., as.collection=F)
													names(vars) = self$fields(col.row.name)
													vars = vars[!sapply(vars, is.null)]
													all.vars = vars %>% unlist %>% unique %>% sort
													ret = sparseMatrix(
														i = sapply(1:length(vars), simplify=F, function(n){rep(n, length(vars[[n]]))}) %>% unlist,
														j = sapply(vars, simplify=F, function(x){match(x, all.vars)}) %>% unlist,
														dimnames = list(
															object = names(vars),
															variation = all.vars
														)
													)
												} else if (attribute[[1]] == "baf"){
													mybafs = self$each("bafs", dp = list(raw=sprintf("dp >= %d", min.dp)), as.collection=F)
													vars = mybafs %>% sapply(. %>% extract2('variation_id'))
													names(vars) = self$each("id", F)
													vars = vars[!sapply(vars, is.null)]
													all.vars = vars %>% unlist %>% unique %>% sort
													# TODO Hier muss noch das combine.baf rein, falls andere ebenenen genutzt werden.
													ret = sparseMatrix(
														i = sapply(1:length(vars), simplify=F, function(n){rep(n, length(vars[[n]]))}) %>% unlist,
														j = sapply(vars, simplify=F, function(x){match(x, all.vars)}) %>% unlist,
														x = mybafs %>% sapply(. %>% extract2('baf')) %>% unlist,
														dimnames = list(
															object = names(vars),
															variation = all.vars
														)
													)
												} else {
													stop("Not a valid attribute label")
												}
												invisible(ret)
											},
											distance.matrix = function(measure = c("cosine", "eucledian", "tanimoto", "jaccard", "tanimoto.similarity"), attribute = c("present", "baf"), min.dp = 100, col.row.name = "id", ...){
												mat = self$variation.matrix.quick(attribute = attribute, min.dp=min.dp, col.row.name = col.row.name, ...)
												if (measure[[1]] == "eucledian"){
													ret = dist(mat) %>% as.matrix
												} else if (measure[[1]] == "cosine"){
													ret = (1-private$cosine.similarity(mat)) #%>% as.dist
												}else if (measure[[1]] == "tanimoto"){
													ret = private$tanimoto.distance(mat)
												}else if (measure[[1]] == "jaccard"){
													ret = private$jaccard.distance(mat)
												}else if (measure[[1]] == "tanimoto.similarity"){
													ret = 1-private$tanimoto.similarity(mat)
												} else {
													stop("similarity measure not supported")
												}
												ret
											},
											print = function(..., print.collection = F, indent = "") {
												cat(sprintf("%s<%s>\n", indent, class(self)[[1]]))
												cat(sprintf("%s\t KLASS: %s\n", indent, self$klass$classname))
												cat(sprintf("%s\t #IDS: %d\n", indent, self$size))
												if (print.collection)
													for(i in 1:length(self$collection)){
														if (i > 10){
															cat(sprintf("%s\t [[%d more elements...]]\n", indent, (self$size - 10)))
															break
														}
														cat(sprintf("%s\t [[%d]]\n", indent, i))
														print(self$collection[[i]], ..., indent = "\t |\t")
													}
												invisible(self)
											}
										 ),
										 active = list(
										 	id = function(value){
										 		self$fields("id")
										 	},
										 	records = function(value){
										 		sapply(self$collection, simplify=F, function(x){
										 			x$record
										 		}) %>% rbindlist
										 	},
										 	record = function(value){
										 		self$records()
										 	},
										 	size = function(value){
										 		length(self$collection)
										 	}
										 ),
										 private = list(
										 	cosine.similarity = function(mat){
										 		mat.hlp = rowSums(mat^2) %>% sqrt
										 		mat.norm = mat / mat.hlp
										 		mat.sim = mat.norm %*% t(mat.norm)
										 		mat.sim
										 	},
										 	absolute.overlap = function(mat){
										 		## matrix ops...
										 		#mat = matrix(c(
										 		#	1, 0, 1, 1,
										 		#	1, 0, 0, 1,
										 		#	1, 1, 1, 1
										 		#), ncol = 4, byrow=T)
										 		# TODO we need to check if the matrix is boolean or not.
										 		abs.overlap = mat %*% t(mat)
										 		#      [,1] [,2] [,3]
										 		#[1,]    3    2    3
										 		#[2,]    2    2    2
										 		#[3,]    3    2    4
										 		abs.overlap
										 	},
										 	relative.overlap = function(mat){
										 		abs.overlap = private$absolute.overlap(mat)
										 		rel.overlap = abs.overlap / diag(abs.overlap)
										 		#     [,1] [,2] [,3]
										 		#[1,] 1.00 0.66    1
										 		#[2,] 1.00 1.00    1
										 		#[3,] 0.75 0.50    1
										 		rel.overlap
										 	},
										 	tanimoto.similarity = function(mat){
										 		rel.overlap = private$relative.overlap(mat)
										 		# This one is asymetric and we dont want that
										 		# we take the paralell minimun of all elements on the upper and lower triangle of the matrix to normalize
										 		mat.tani = pmin(rel.overlap[upper.tri(rel.overlap)], rel.overlap[lower.tri(rel.overlap)])
										 		rel.overlap[upper.tri(rel.overlap)] <- mat.tani
										 		rel.overlap[lower.tri(rel.overlap)] <- mat.tani
										 		rel.overlap
										 	},
										 	not.matrix = function(mat){
										 		if (inherits(m, "ngCMatrix")) {

										 		} else if (inherits(x, "lgeMatrix")) {
										 			!mat
										 		} else {
										 			abs(mat-1)
										 		}
										 	},
										 	jaccard.distance = function(mat){
										 		mat.neg = private$not.matrix(mat)
										 		A = private$absolute.overlap(mat)
										 		D = private$absolute.overlap(mat.neg)
										 		n = ncol(mat)
										 		1-(A/(n-D))
										 	},
										 	tanimoto.distance = function(mat){
										 		mat.neg = private$not.matrix(mat)
										 		A = private$absolute.overlap(mat)
										 		D = private$absolute.overlap(mat.neg)
										 		n = ncol(mat)
										 		1-((A+D)/(2*n-A-D))
										 	},
										 	## Similarity functions have to be able to cope with binary vectors! Not with sets!
										 	cosine.similarity.vec = function(a,b){
										 		if (is.logical(a)) a = as.integer(a)
										 		if (is.logical(a)) b = as.integer(b)
										 		nenner = (a %*% b)
										 		zaehler = (sqrt(a%*%a * b%*%b))
										 		if (zaehler == 0) zaehler = -Inf
										 		ret = nenner / zaehler
										 		ret
										 	}
										 )
)

#'@export [[.SnupyDataCollection
`[[.SnupyDataCollection` <-
	function(x, i, j, ...)
	{
		if (missing(j)){
			x$collection[[i]]
		}else if (missing(...)){
			x$collection[[i, j]]
		} else {
			x$collection[[i, j, ...]]
		}
	}

#'@export [.SnupyDataCollection
`[.SnupyDataCollection` <-
	function(x, i, j, ...)
	{
		if (missing(j)){
			x$collection[i]
		}else if (missing(...)){
			x$collection[i, j]
		} else {
			x$collection[i, j, ...]
		}
	}


