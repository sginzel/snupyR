#' Get a Aqua object
#'
#'\code{Aqua} generates a S6 class
#'
#'@usage
#' Aqua$new(session = ssion, use.defaults = T)
#'
#'@details Aqua provides methods to cache the result of functions calls.
#'
#'@section Methods:
#'These methods are accessible:
#'
#'\describe{
#' \item{\code{store(file = NULL)}}{Saves cached values to file}
#' }
#'@return Object of R6 class which hold connection information
#'@importFrom R6 R6Class
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{SnupySession}}, \code{\link{snupy.query}}, \code{\link{snupy.request}}
#'@examples
#' aqua = Aqua$new(session = ssion, use.defaults = T)
#' aqua$queries = NULL #reset all queries
#' aqua$filters = NULL #reset all filters
#' Aqua$listAggregation(ssion)
#' Aqua$annotations(session = ssion, variation_ids = 1, organism_id = 1, model = "Vep", attributes = NULL)
#' akeys = c(
#'  'aggregation_variation_call:variation_calls.variation_id',
#' 	'aggregation_variation_call:variation_calls.dp',
#' 	'aggregation_annovar:annovars.organism_id',
#' 	'aggregation_annovar:annovars.ensembl_annotation',
#' 	'aggregation_annovar:annovars.ensembl_gene'
#' )
#' # fkey
#' qkeys = list(
#' 	"query_consequence:consequence" = c("missense_variant", "stop_gained"),
#' 	#"query_consequence:consequence[]" = "missense_variant",
#' 	#"query_consequence:consequence[]" = "stop_gained",
#' 	"query_variation_call:read_depth" = 100,
#' 	"query_simple_population:population_frequency" = 0.01
#' )
#' fkeys = c('vep_filter_consequence:consequence_severe', 'filter_variation_call:vcdp', 'vep_filter_variant:vep_onekg', 'vep_filter_variant:vep_exac')
#'
#' eg = EntityGroup$new(3443, ssion)
#' sc = eg$samples()
#' aqua = Aqua$new(ssion)
#' aqua$query_and_aggregate(sc)
#' res = aqua$query(sc)
#' agg = aqua$aggregate(res)
#'@aliases aqua
#'@family cache
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
Aqua <- R6Class("Aqua",
										 public = list(
										 	session = NULL,
										 	initialize = function(session, use.defaults = T) {
										 		self$session <- session
										 		stopifnot(!is.null(self$session))
										 		if (use.defaults){
										 			self$filters <- c('vep_filter_consequence:consequence_severe', 'filter_variation_call:vcdp', 'vep_filter_variant:vep_onekg', 'vep_filter_variant:vep_exac')
										 			self$queries <- list(
										 				"query_consequence:consequence" = Aqua$get_default("query_consequence:consequence", self$session),
										 				"query_variation_call:read_depth" = 20,
										 				"query_simple_population:population_frequency" = list(value = 0.01, combine = "AND")
										 			)
										 			self$aggregations <- c('aggregation_samples:samples.name', 'aggregation_samples:samples.id', 'aggregation_samples:samples.specimen_probe_id',
										 														 'aggregation_variation_call:variation_calls.variation_id', 'aggregation_variation_call:variation_calls.gt', 'aggregation_variation_call:variation_calls.ref_reads', 'aggregation_variation_call:variation_calls.alt_reads', 'aggregation_variation_call:variation_calls.gq',
										 														 'aggregation_vep:vep_ensembls.consequence', 'aggregation_vep:vep_ensembls.exac_adj_allele', 'aggregation_vep:vep_ensembls.gene_symbol', 'aggregation_vep:vep_ensembls.transcript_id', 'aggregation_vep:vep_ensembls.canonical',
										 														 'aggregation_annovar:annovars.cadd_phred', 'annovars.phylop100way_vertebrate'
										 														 )
										 		}
										 		invisible(self)
										 	},
										 	query_and_aggregate = function(sample_collection, ..., cache.use = F, only.default.consequence = F){
										 		res = self$query(sample_collection, ..., cache.use = cache.use)
										 		if (!res$error %>% is.null){
										 			return(res)
										 		}
										 		agg = self$aggregate(res, only.default.consequence = only.default.consequence)
										 		list(
										 			query = res,
										 			aggregation = agg
										 		)
										 	},
										 	query = function(sample_collection, ..., cache.use = F){
												qparams = self$build_query()
												qparams$samples = sample_collection$ids
												if (self$session$SHOWSQL){
													qparams$sql = 1
												}
												result = snupy.request(resource = "aqua/query", params=qparams, format="json", session = self$session, cache.use = cache.use)
												result
										 	},
										 	aggregate = function(query_resut, only.default.consequence = F){
										 		if (is.null(query_resut$error)){
										 			#if (1 == 0){
										 			#aggparams = snupy.request(resource = "aqua/build_query", params=list(akey=private$.aggregations), format="json", session = self$session)
										 			#}
										 			aggparams = self$build_query()
										 			aggparams$samples = query_resut$sample_ids %>% unlist
										 			aggparams$variation_call_ids = query_resut$variation_call_ids

										 			agg = snupy.request(resource = "aqua/aggregation", params=aggparams, format="csv", session = self$session, cache.use = T)
										 			agg %<>% unique
										 			if (only.default.consequence){
										 				# find consequence aggregations
										 				cons.names = grep(".*consequence.*", names(agg), value=T)
										 				default.consequences = private$.queries[["query_consequence:consequence"]]
										 				if (!is.null(default.consequences)){
										 					msk = rep(T, nrow(agg))
										 					for (cname in cons.names){
										 						msk = msk & agg[[cname]] %in% default.consequences
										 					}
										 					agg = agg[msk,]
										 				}
										 			}
										 		} else {
										 			query_resut$error
										 		}
										 		agg
										 	},
										 	build_query = function(sample_collection_or_query_result = NULL){
										 		# check if fiven keys are valid
										 		if (length(private$.queries) > 0){
										 			query_valid = private$.queries %>% names %>% unlist %>% `%in%`(., Aqua$listFilters(self$session)$qkey) %>% all
										 			if (!all(query_valid)){
										 				cat(sprintf("Query not found: %s", paste0(private$.queries[!query_valid], collapse=",")))
										 			}
										 			stopifnot(all(query_valid))
										 		}
										 		if (length(private$.filters) > 0){
										 			filter_valid = private$.filters %>% unlist %>% `%in%`(., Aqua$listFilters(self$session)$fkey)
										 			if (!all(filter_valid)){
										 				cat(sprintf("Filter not found: %s", paste0(private$.filters[!filter_valid], collapse=",")))
										 			}
										 			stopifnot(all(filter_valid))
										 		}
										 		if (length(private$.aggregations) > 0){
										 			aggregation_valid = private$.aggregations %>% unlist %>% `%in%`(., Aqua$listAggregation(self$session)$akey) %>% all
										 			if (!all(aggregation_valid)){
										 				cat(sprintf("Aggregation not found: %s", paste0(private$.aggregations[!aggregation_valid], collapse=",")))
										 			}
										 			stopifnot(all(aggregation_valid))
										 		}
										 		template = snupy.request(resource = "aqua/build_query",
										 														 params=list(
										 														 	qkey=private$.queries,
										 														 	fkey=private$.filters,
										 														 	akey=private$.aggregations
										 														 ),
										 														 format="json",
										 														 session = self$session)
										 		if (!sample_collection_or_query_result %>% is.null){
										 			if (sample_collection_or_query_result$error %>% is.null){
											 			if (sample_collection_or_query_result %>% inherits(., "SampleCollection")){
											 				template$samples = sample_collection_or_query_result$ids
											 			} else {
											 				if (!sample_collection_or_query_result[["sample_ids"]] %>% is.null)
											 					template$samples = sample_collection_or_query_result[["sample_ids"]]
											 				if (!sample_collection_or_query_result[["variation_call_ids"]] %>% is.null)
											 					template$variation_call_ids = sample_collection_or_query_result[["variation_call_ids"]]
											 			}
										 			} else {
										 				cat(sample_collection_or_query_result$error)
										 				stop("Error detected during the process...")
										 			}
										 		}
										 		return(template)
										 	}
										 ),
										 active = list(
										 	queries = function(qlist){
										 		if (!missing(qlist)){
										 			if (any(names(qlist) %in% private$.queries)){
										 				warning("Some query parameters were set previously. Replacing previous query parameters.")
										 				private$.queries[names(private$.queries) %in% names(qlist)] = NULL
										 			}
										 			if (length(qlist) > 0 && !is.null(qlist)){
										 				private$.queries <- c(private$.queries, qlist)
										 			} else {
										 				private$.queries = c()
										 			}
										 		}

										 		return(private$.queries)
										 	},
										 	filters = function(fkey){
										 		if (!missing(fkey)){
										 			if (length(fkey) > 0 && !is.null(fkey)){
										 				private$.filters <- c(private$.filters, fkey) %>% unlist %>% unique
										 			} else {
										 				private$.filters = c()
										 			}
										 		}

										 		return(private$.filters)
										 	},
										 	aggregations = function(akey){
										 		if (!missing(akey)){
										 			if (length(akey) > 0 && !is.null(akey)){
										 				private$.aggregations <- c(private$.aggregations, akey) %>% unlist %>% unique
										 			} else {
										 				private$.aggregations = c()
										 			}
										 		}
										 		return(private$.aggregations)
										 	}
										 ),
										 private = list(
										 	.filters = c(),
										 	.aggregations = c(),
										 	.queries = c(),
										 	configure_query_X = function(...){
										 		cfg = list(...)
										 		qkeys = subset(Aqua$listFilters(session = self$session), fkey %in% private$.filters)$qkey %>% unique

										 		template = snupy.request(resource = "aqua/build_query",
										 														 params=list(
										 														 	qkey=private$.queries,
										 														 	fkey=private$.filters,
										 														 	akey=private$.aggregations
										 														 ),
										 														 format="json",
										 														 session = self$session)
										 		return(template)
										 	}
										 )
)
Aqua$listAnnotations <- function(session){
	snupy.request(resource = "aqua/listAnnotation", format="csv", session = session, cache.use = T, http.method = GET)
}
Aqua$listFilters <- function(session){
	snupy.request(resource = "aqua/listFilter", format="csv", session = session, cache.use = T, http.method = GET)
}
Aqua$listQuery <- function(session){
	snupy.request(resource = "aqua/listQuery", format="csv", session = session, cache.use = T, http.method = GET)
}
Aqua$listAggregation <- function(session){
	snupy.request(resource = "aqua/listAggregation", format="csv", session = session, cache.use = F, http.method = GET)
}

Aqua$get_default <- function(querykey, session){
	if (missing(querykey)){
		sapply(Aqua$listQuery(session)$qkey, simplify=F, .%>% Aqua$get_default(querykey = ., session)) %>%
		return
	}
	queries = Aqua$listQuery(session)
	defaults = subset(queries, qkey == querykey)$default
	if (!is.na(defaults)){
		type = subset(queries, qkey == querykey)$type
		default = switch(type,
										checkbox = defaults[[1]],
										collection = defaults[[1]] %>% gsub('"', '', .) %>% strsplit(., ",") %>% extract2(1),
										delimtext = defaults[[1]] %>% gsub('"', '', .) %>% strsplit(., ",") %>% extract2(1),
										double = defaults[[1]] %>% as.double,
										number = defaults[[1]] %>% as.integer,
										numeric = defaults[[1]] %>% as.double,
										range_gt = defaults[[1]] %>% gsub('"', '', .) %>% strsplit(., ",") %>% extract2(1) %>% extract2(3),
										range_lt = defaults[[1]] %>% gsub('"', '', .) %>% strsplit(., ",") %>% extract2(1) %>% extract2(3),
										select = defaults[[1]] %>% gsub('"', '', .) %>% strsplit(., ",") %>% extract2(1) %>% extract2(1),
										text = defaults[[1]],
										textarea = defaults[[1]],
										NULL
									 )
	} else {
		default = NULL
	}
	default
}

#'@export
Aqua$annotations <- function(session, variation_ids, organism_id, modelname, attributes = NULL, where = NULL){
	if (!is.null(attributes)){
		available_attrs = subset(Aqua$listAnnotations(session = ssion), model == modelname)$attributes %>% strsplit(., ",") %>% extract2(1)
		if (any(!attributes %in% available_attrs))
			stop("Not all attributes are available for this model")
	}
	if (is.list(where)){
		where = get_sql_where(condition.list = where)
	}
	result = snupy.request(resource = "aqua/annotation",
								format="csv",
								params=list(
									varids=variation_ids,
									organism=organism_id,
									model=modelname,
									attributes= attributes,
									where=where
								),
								session = session,
								cache.use = F,
								http.method = POST)
	if (!is.null(result$error))
		return(data.table(error=result$error))
	result
}

#'@export Ops.Aqua
Ops.Aqua <- function(c1, c2){
	op = .Generic[[1]]
	switch(op,
				 `==` = {
				 	stop("not implemented")
				 },
				 `!=` = {
				 	! (x == y)
				 },
				 `+` = {
				 	new_aqua = c1$clone
				 	new_aqua$filters <- c2$filters
				 	new_aqua$aggregations <- c2$aggregations
				 	new_aqua$queries <- c2$queries
				 	new_aqua
				 },
				 stop("undefined operation")
	)
}
#Ops.AquaVep <- Ops.Aqua
#Ops.AquaVepRefSeq <- Ops.Aqua
#Ops.AquaAnnovar <- Ops.Aqua
#Ops.AquaSnpEff <- Ops.Aqua
