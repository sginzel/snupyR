#' Get and Sample object
#'
#'@include snupy_data_collection_classes.R
#'\code{Sample} generates a S6 class
#'
#'@importFrom R6 R6Class
#'@include snupy_data.R
#'@param filepath a path to a .r file that contains the config
#'@param connect logical value whether ssh tunnel should be connected or not.
#'@details The file containig the configuration should set the following variables\cr
#'\describe{
#'	\item{name}{logical value if debug information should be shown}
#'	\item{tags}{tags linked to this entity group}
#'}
#'@section Methods:
#'@return Object of R6 class to use Samples
#'@field name Name of the \code{\link{Sample}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the Sample has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' Sample$new(experiment_id=2)
#' Sample$new(experiment=Experiment$new(2))
#' Sample$new(id=123)
#'@examples
#' eg <- Sample$new(experiment_id=2)
#'@aliases sample
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
Sample <- R6Class("Sample",
									inherit = SnupyData,
									public = list(
										table = "samples",
										collection_klass = SampleCollection,
										initialize = function(id, session, ...) {
											super$initialize(id, "Sample", session, "samples", ...)
										},
										variation_calls = function(..., refresh = FALSE, cache.use = TRUE, full = FALSE){
											params = suppressWarnings(digest(list(...) %>% unlist %>% sort, "crc32", ascii=T))
											columns_to_retrieve = `if`(!full,
																								 c("id AS variation_call_id",
																								 	"variation_id",
																								 	"sample_id",
																								 	"dp",
																								 	"(alt_reads/(alt_reads+ref_reads)) AS baf"),
																								 c(
																								 	"id AS variation_call_id",
																								 	"variation_id",
																								 	"sample_id",
																								 	"qual",
																								 	"dp",
																								 	"gt",
																								 	"gq",
																								 	"alt_reads",
																								 	"ref_reads",
																								 	"(alt_reads/(alt_reads+ref_reads)) AS baf")
											)
											buildStatement(
												table = "variation_calls",
												columns = columns_to_retrieve,
												sample_id = self$id,
												...
											) %>% snupy.query(., session = self$session, cache.use = ifelse(cache.use, sprintf("variation_calls_sample_%s_FULL_%s_PARAMS_%s", self$id, as.character(full), params), NULL), cache.overwrite = refresh)
										},
										variation_id = function(..., refresh = FALSE, cache.use = TRUE){
											self$variation_calls(..., refresh = refresh, cache.use = cache.use, full = F)$variation_id
										},
										variations = function(refresh = FALSE, ..., cache.use = TRUE){
											vc = self$variation_calls(..., refresh = refresh, cache.use = cache.use, full = F)
											extract2(vc, 'variation_id') %>% set_names(vc$variation_call_id)
										},
										bafs = function(..., refresh = FALSE, cache.use = TRUE, return.vector = FALSE, combine.baf = NULL){
											ret = self$variation_calls(..., refresh = refresh, cache.use = cache.use, full = F)
											if (return.vector){
												ret = ret %>% extract2("baf") %>% set_names(ret$variation_id)
											}
											ret %>% invisible
										},
										myprivates = function(){}
									),
									active = list(
										entity_groups = function(newval){
											if (private$.entity_groups %>% is.null)
												private$.entity_groups = `if`(self$field("specimen_probe_id") %>% is.null,
																											EntityGroupCollection$new(NULL, self$session),
																											EntityGroupCollection$new(
																												buildStatement(
																													table = "entities",
																													columns = "entity_group_id",
																													joins = list(
																														specimen_probes = c("specimen_probes.entity_id", "entities.id")
																													),
																													specimen_probes.id = self$field("specimen_probe_id")
																												) %>% snupy.query(., session = self$session, extract="entity_group_id", cache.use = T) %>% sort %>% unique
																												, self$session
																											)
												)

											private$.entity_groups
										},
										entities = function(newval){

											if (private$.entities %>% is.null)
												private$.entities = `if`(self$field("specimen_probe_id") %>% is.null,
																								 EntityCollection$new(NULL, self$session),
																								 EntityCollection$new(
																								 	buildStatement(
																								 		table = "specimen_probes",
																								 		columns = "specimen_probes.entity_id as id",
																								 		id = self$field("specimen_probe_id")
																								 	) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
																								 	, self$session)
												)

											private$.entities
										},
										specimen_probes = function(newval){

											if (private$.specimen_probes %>% is.null)
												private$.specimen_probes = `if`(self$field("specimen_probe_id") %>% is.null,
																												SpecimenProbeCollection$new(NULL, self$session),
																												SpecimenProbeCollection$new(
																													buildStatement(
																														table = "specimen_probes",
																														columns = "specimen_probes.id as id",
																														specimen_probes.id = self$field("specimen_probe_id")
																													) %>% snupy.query(., session = self$session, extract="id", cache.use = T) %>% sort %>% unique
																													, session = self$session
																												)
												)

											private$.specimen_probes
										},
										samples = function(newval){
											if (private$.samples %>% is.null)
												private$.samples = SampleCollection$new(self$id, self$session)
											private$.samples
										},
										vcf_files = function(newval){

											if (private$.vcf_files %>% is.null)
												private$.vcf_files = VcfFileCollection$new(
													buildStatement(
														table = "vcf_files",
														columns = "vcf_files.id as id",
														vcf_files.id = self$field("vcf_file_id")
													) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
													, self$session
												)

											private$.vcf_files
										}
									)
)
Sample$object_type = "Sample"
Sample$all = function(session, ids = NULL){
	SnupyData$all("samples", ids = ids, session = session, global.lookup = T)
}

#' SnupySampleTag
#' @export
SampleTag <- R6Class("SampleTag",
										 inherit = SnupyTag,
										 public = list(
										 	initialize = function(id, session) {
										 		private$build(id, session, "Sample")
										 	}
										 )
)


