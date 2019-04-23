#' Get a SnupyData object
#'
#'\code{SnupyData} generates a S6 class
#'
#'@usage
#' SnupyData$new(cache.path = NULL, verbose = FALSE)
#'@param cache.path path to file that is used to store the cache if requested.
#'@param verbose logical value
#'
#'@details SnupyData provides methods to cache the result of functions calls.
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
#' > mem = SnupyData$new(verbose = T)
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
SnupyData <- R6Class("SnupyData",
											 public = list(
											 	object_type = NULL,
											 	id = NULL,
											 	table = NULL,
											 	columns.to.load = NULL,
											 	session = NULL,
											 	record = NULL,
											 	klass = NULL,
											 	collection_klass = NULL,
											 	initialize = function(id, object_type, session, table = NULL, template = NULL, columns.to.load = NULL) {
											 		self$id <- id
											 		self$object_type <- object_type
											 		self$klass <- eval(parse(text=object_type))
											 		self$columns.to.load = columns.to.load
											 		self$session <- session
											 		if (is.null(table)){
											 			self$table <- gsub("([[:alnum:]])([A-Z])", "\\1_\\2", object_type) %>% tolower %>% cat(., "s", sep="")
											 		} else {
											 			self$table <- table
											 		}
											 		if (!is.null(template)){
											 			self$record = template
											 		} else {
											 			self$record = private$get_record(id, session)
											 		}
											 		stopifnot(!is.null(self$session))
											 		invisible(self)
											 	},
											 	full_join = function(){
											 		list()
											 	},
											 	columns = function(){
											 		snupy.get_columns(self$table, self$session) %>% grep(paste0("`", self$columns.to.load, "`", collapse = "|"), ., value=T)
											 	},
											 	field = function(name){
											 		self$record[[name]]
											 	},
											 	tags = function(tag_level = c("self", "entity_groups", "entities", "specimen_probes", "samples", "vcf_files")){
											 		if (tag_level[[1]] == "self"){
											 			SnupyTag$find.by.object(self, self$session)
											 		} else {
											 			self[[tag_level[[1]]]]$each("tags") %>% Reduce("+", .)
											 		}
											 	},
											 	has_tag = function(tagvalue = NULL, tag_level = c("self", "entity_groups", "entities", "specimen_probes", "samples", "vcf_files")){
											 		#sapply(self$tags(tag_level), simplify=F, function(tgs){tgs$include?(tagvalue)})
											 		self$tags(tag_level)$include(tagvalue)
											 	},
											 	variation_calls = function(){},
											 	variations = function(){},
											 	variation_id = function(){},
											 	bafs = function(){},
											 	object_size = function(){
											 		object.size(self) %>% format(units="auto")
											 	},
											 	print = function(..., print.fields = F, indent = "") {
											 		cat(sprintf("%s<%s>\n", indent, class(self)[[1]]))
											 		cat(sprintf("%s\t ID: %s\n", indent, self$id))
											 		cat(sprintf("%s\t OBJECT_TYPE: %s\n", indent, self$object_type))
											 		cat(sprintf("%s\t TABLE: %s\n", indent, self$table))
											 		cat(sprintf("%s\t SESSION: %s@%s\n", indent, self$session$USER, self$session$URL))
											 		if (print.fields){
											 			cat(sprintf("%s\t FIELDS:\n", indent))
														for(fname in names(self$record)){
															cat(sprintf("%s\t\t %s: %s\n", indent, fname, gsub("\n", "\\n", paste0(self$field(fname), collapse=","))))
														}
												 	} else {
												 		cat(sprintf("%s\t FIELDS: %s\n", indent, paste0(names(self$record), collapse=", ")))
												 	}
											 		invisible(self)
											 	}
											 ),
										 	 active = list(
										 	 	experiments = function(newval){},
										 	 	entity_groups = function(newval){},
										 	 	entities = function(newval){},
										 	 	specimen_probes = function(newval){},
										 	 	samples = function(newval){},
										 	 	vcf_files = function(newval){},
										 	 	find_tags_to_delete = function(tagvalue) {
										 	 		if (is.null(private$.tags)) {
										 	 			private$.tags = SnupyTagCollection$new(self, self$session)
										 	 		}
										 	 		if (missing(tagvalue)){
										 	 			private$.tags
										 	 		} else {
										 	 			private$.tags$include(tagvalue)
										 	 		}
										 	 	}
										 	 ),
											 private = list(
											 	.tags = NULL,
											 	.experiments = NULL,
											 	.entity_groups = NULL,
											 	.entities = NULL,
											 	.specimen_probes = NULL,
											 	.samples = NULL,
											 	.vcf_files = NULL,
											 	.variation_calls = NULL,
											 	get_record = function(id, session){
											 		self$klass$all(session, ids = id)
											 	}
											 )
)
SnupyData$all <- function(table, session, ids = NULL, columns = snupy.get_columns(table, session), cache.use = TRUE, cache.overwrite = FALSE, global.lookup = FALSE){
	if (global.lookup && !is.null(session$CACHE)){
		global.lookup.key = sprintf("%s.lookup", table)
		if (session$CACHE$get(global.lookup.key) %>% is.null){ # initialize the global lookup
			session$CACHE$add(global.lookup.key,
													SnupyData$all(table, session, ids = NULL, columns = columns,
																				cache.use = FALSE, cache.overwrite = cache.overwrite,
																				global.lookup = FALSE))
		} else { # extend the global lookup, if needed
			if (!is.null(ids)){
				loaded = subset(session$CACHE$get(global.lookup.key), id %in% ids)
				missing.ids = ids[!ids %in% loaded$id]
				if (missing.ids %>% length > 0){
					missing.records = SnupyData$all(table, session, missing.ids, columns = columns,
																				cache.use = FALSE, cache.overwrite = cache.overwrite,
																				global.lookup = FALSE)
					session$CACHE$add(global.lookup.key, rbind(session$CACHE$get(global.lookup.key), missing.records))
				}
			}
		}
		if (!is.null(ids))
			return(session$CACHE$get(global.lookup.key)[id %in% ids,])
		return(session$CACHE$get(global.lookup.key))
	}
	if (is.null(ids)){
		statement = buildStatement(
			table = table,
			columns = columns
		)
	} else {
		statement = buildStatement(
			table = table,
			columns = columns,
			id = ids
		)
	}
	ret = snupy.query(statement, session, cache.use = cache.use, cache.overwrite = cache.overwrite)
	if (!is.null(ids) && nrow(ret) != length(unique(ids))){
		warning(sprintf("Not all objects could be retrieved (%d/%d)", nrow(ret), length(unique(ids))))
	}

	ret
}

#'@export as.list.SnupyDataCollection
as.list.SnupyData <- function(obj, ...){as.list(sdc$record, ...)}

#'@export as.vector.SnupyDataCollection
as.vector.SnupyData <- function(obj, mode){as.vector(as.list(obj))}

#'@export Ops.SnupyData
Ops.SnupyData <- function(c1, c2){
	op = .Generic[[1]]
	switch(op,
				 `==` = {
				 	(c1$object_type) == (c2$object_type) &
				 	(c1$id == c2$id)
				 },
				 `!=` = {
				 	! (x == y)
				 },
				 `+` = {
				 	if (c1 == c2){
				 		c1$collection_klass$new(c(c1$id, c2$id), c1$session)
				 	} else {
				 		stop("Cant join different snupy data types")
				 	}
				 },
				 stop("undefined operation")
	)
}
Ops.EntityGroup <- Ops.SnupyData
Ops.Entity <- Ops.SnupyData
Ops.SpecimenProbe <- Ops.SnupyData
Ops.Sample <- Ops.SnupyData
Ops.VcfFile <- Ops.SnupyData
