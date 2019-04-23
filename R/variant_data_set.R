#' Use a VariantDataSet module to filter variation calls
#'
#'\code{VariantDataSet} generates a S6 class
#'
#'@importFrom R6 R6Class
#'@param collection_or_sample_ids a collection of ids or a vector of sample ids
#'@param session session object
#'@details Variant calls are retrieved seperately for each sample, when needed.
#'@return VariantDataSet
#'@field session() session object
#'@field variation_calls() table of variation calls
#'@field samples() returns the sample objects
#'@field variants list of variant ids
#'@field samples_ids returns a vector of sample_ids
#'@section Methods:
#'  \describe{
#'   \item{\code{regions(include.alterations = F, as.ranges = T)}}{Returns the coordinates of the variants in the dataset. Optionally as GRanges objects(not implemented)}
#'   \item{\code{variation.matrix(attribute = c("present", "baf"), min.dp = 100, col.row.name = "id")}}{A sample_id-variation matrix, possibly filtered for read depth. Cells are binary (present) or are set to the variation frequency.}
#'   \item{\code{aqua(aqua=NULL)}}{performs aqua aggregation for variants in the set, in case the variants are already filtered.}
#'   \item{\code{+,-,&,|}}{Performs set operations on variant datasets. Returns a new variant dataset each time.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' VariantDataSet$new(sample_collection_or_sample_ids, session)
#'@examples
#' VariantDataSet$new(c(1,2), ssion)
#'@aliases VariantCallSet
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
VariantDataSet <- R6Class("VariantDataSet",
								public = list(
									session = NULL,
									initialize = function(collection_or_sample_ids, session) {
										self$session <- session
										if (!inherits(collection_or_sample_ids, "SnupyDataCollection")){
											if (is.list(collection_or_sample_ids)){
												# might be a list of sample collections
												sample_ids = sapply(collection_or_sample_ids, simplify = F, function(x){
													if (inherits(x, "SnupyDataCollection")){
														x$each("samples") %>% sapply(., simplify=F, function(x){x$ids})
													} else {
														x
													}
												}) %>% unlist %>% unique
											} else {
												sample_ids = collection_or_sample_ids %>% unlist %>% unique
											}
										} else {
											sample_ids = collection_or_sample_ids$each("samples") %>% sapply(., simplify=F, function(x){x$ids}) %>% unlist %>% unique
										}
										private$.samples = SampleCollection$new(sample_ids, self$session)
										stopifnot(!is.null(self$session))
										invisible(self)
									},
									variation_calls = function(){
										private$.samples$each("variation_calls", variation_id = private$.variants) %>% rbindlist
									},
									filter = function(variation_ids = NULL, sample_ids = NULL){
										vc = self$variation_calls()
										if (!is.null(variation_ids)){
											private$.variants = unique(subset(vc, variation_id %in% variation_ids)$variation_id)
										}
										if (!is.null(sample_ids)){
											# make sure we dont keep variants from a sample that was excluded
											private$.variants = unique(subset(vc, sample_id %in% sample_ids)$variation_id)
											private$.sample = private$.samples$select(ids = sample_ids)
										}
									},
									regions = function(include.alteration = F, as.ranges = F){
										ret = buildStatement(
											table = "variations",
											joins = list(
												regions = c("regions.id", "variations.region_id")
											),
											columns = c("variations.id AS variation_id, regions.name as chr, regions.start AS cstart, regions.stop AS cstop"),
											variations.id = self$variants
										) %>% snupy.query(., session = self$session, cache.use = T)
										if (include.alteration){
											alt_ids = buildStatement(
												table = "variations",
												columns = c("variations.id as variation_id, alteration_id"),
												variations.id = self$variants
											) %>% snupy.query(., session = self$session, cache.use = T)
											alt_values = buildStatement(
												table = "alterations",
												columns = c("id as alteration_id, ref, alt"),
												alterations.id = unique(alt_ids$alteration_id)
											) %>% snupy.query(., session = self$session, cache.use = T)
											alt_ids = merge(alt_ids, alt_values, by="alteration_id")
											ret = merge(ret, alt_ids, by="variation_id")
											rm(alt_ids)
											rm(alt_values)
										}
										if (as.ranges){
											stop("not implemented to convert regions to GRanges")
										}
										ret
									},
									variation.matrix = function(attribute = c("present", "baf"), min.dp = 100, col.row.name = "id", ...){
										private$.samples$variation.matrix(attribute = attribute, min.dp = min.dp, col.row.name = col.row.name, "variation_calls.variation_id" = private$.variants)
									},
									aqua = function(aqua = NULL, ...){
										if (aqua %>% is.null){
											aqua = Aqua$new(self$session, use.defaults = T)
										} else {
											aqua = aqua$clone()
										}
										aqua$queries <- list("query_batch:variation_calls.variation_id" = self$variants)
										aqua$filters <- "filter_batch:variation_calls.variation_id"
										aqua$query_and_aggregate(private$.samples, ...)
									},
									aqua.aggregate = function(aqua = NULL){
										if (aqua %>% is.null){
											aqua = Aqua$new(self$session, use.defaults = T)
										} else {
											aqua = aqua$clone()
										}
										aqua$aggregate(self$variation_calls()$id)
									},
									samples = function(){
										private$.samples
									}
								),
								active = list(
									variants = function(newval){
										if (!missing(newval))
											private$.variants = newval
										if (private$.variants %>% is.null){
											private$.variants = buildStatement(
												table = "variation_calls",
												columns = c("DISTINCT(variation_id) AS variation_id"),
												sample_id = self$sample_ids
											) %>% snupy.query(., session = self$session, cache.use = T, extract="variation_id")
										}
										private$.variants
									},
									sample_ids = function(newval){
										if (!missing(newval))
											private$.samples = SampleCollection$new(newval, self$session)
										private$.samples$ids %>% unique
									},
									size = function(newval){
										length(private$.variants)
									}
								),
								private = list(
									.variants = NULL,
									.samples = NULL
								)
)

#'@export Ops.VariantDataSet
Ops.VariantDataSet <- function(vs1, vs2){
	op = .Generic[[1]]
	switch(op,
				 `==` = {
				 	all(vs1$sample_ids %in% vs2$sample_ids) &
			 		all(vs2$sample_ids %in% vs1$sample_ids) &
			 		all(vs1$variants %in% vs2$variants) &
			 		all(vs2$variants %in% vs1$variants)
				 },
				 `!=` = {
				 	! (x == y)
				 },
				 `+` = {
				 	ret = vs1$clone()
				 	ret$sample_ids <- union(vs1$sample_ids, vs2$sample_ids)
				 	ret$variants <- union(vs1$variants, vs2$variants)
				 	ret
				 },
				 `-` = {
				 	ret = vs1$clone()
				 	ret$sample_ids <- union(vs1$sample_ids, vs2$sample_ids)
				 	ret$variants <- setdiff(vs1$variants, vs2$variants)
				 	ret
				 },
				 `|` = {
				 	vs1 + vs2
				 },
				 `&` = {
				 	ret = vs1$clone()
				 	ret$sample_ids <- union(vs1$sample_ids, vs2$sample_ids)
				 	ret$variants <- intersect(vs1$variants, vs2$variants)
				 	ret
				 },
				 stop("undefined operation")
	)
}

#'@export intersect.VariantDataSet
intersect.VariantDataSet <- function(vs1, vs2){vs1 & vs2}

#'@export setdiff.VariantDataSet
setdiff.VariantDataSet <- function(vs1, vs2){vs1 - vs2}

#'@export union.VariantDataSet
union.VariantDataSet <- function(vs1, vs2){vs1 + vs2}
