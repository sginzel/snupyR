#' Get and Entity object
#'
#' @include snupy_data_collection_classes.R
#'\code{Entity} generates a S6 class
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
#'@return Object of R6 class to use Entitys
#'@field name Name of the \code{\link{Entity}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the Entity has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' Entity$new(experiment_id=2)
#' Entity$new(experiment=Experiment$new(2))
#' Entity$new(id=123)
#'@examples
#' eg <- Entity$new(experiment_id=2)
#'@aliases entity
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
Entity <- R6Class("Entity",
									inherit = SnupyData,
									public = list(
										table = "entities",
										collection_klass = EntityCollection,
										initialize = function(id, session, ...) {
											super$initialize(id, "Entity", session, "entities", ...)
										},
										variation_calls = function(refresh, ...){
											(self$specimen_probes$each("variation_calls", as.collection = FALSE) %>% rbindlist)[,entity_id := self$id]
										},
										variations = function(refresh, ...){
											(self$specimen_probes$each("variations", as.collection = FALSE) %>% c) %>% unlist
										},
										bafs = function(refresh = FALSE, ..., return.vector = FALSE, combine.baf = T){
											ret = (self$specimen_probes$each("bafs", as.collection = FALSE, refresh = refresh, return.vector = FALSE, combine.baf = F) %>% rbindlist)[,entity_id := self$id]
											if (combine.baf){
												ret = ret[,list(combine.baf = mean(baf), entity_id = entity_id), by=variation_id] %>% unique
											}
											if (return.vector){
												ret.names = ret$variation_id %>% as.character %>% make.unique
												if (combine.baf){
													ret = ret$combine.baf
												} else {
													ret = ret$baf
												}
												names(ret) = ret.names
											}
											ret %>% invisible
										},
										parents = function(){
											# ssion = SnupySession$new(filepath = "../snupy-aqua.conf.r", cache.name="SNUPY-dev")
											# alps6 = Entity$new(id = 167, session = ssion)
											specimens = self$entity_groups[[1]]$specimen_probes
											parents = specimens$each(function(sp){
												if (any(sp$tags()$field("value") %in% c("CFFTHR", "CFMTHR"))){
													sp$entities[[1]]
												} else {
													NULL
												}
											})
											parents = parents[!sapply(parents, is.null)]
											entcoll = EntityCollection$new(ids = c(), session = self$session)
											entcoll$refresh(parents)
											entcoll
										},
										siblings = function(){
											specimens = self$entity_groups[[1]]$specimen_probes
											siblings = specimens$each(function(sp){
												if (any(sp$tags()$field("value") %in% c("CFSSTR", "CFBRTHR"))){
													sp$entities[[1]]
												} else {
													NULL
												}
											})
											siblings = siblings[!sapply(siblings, is.null)]
											entcoll = EntityCollection$new(ids = c(), session = self$session)
											entcoll$refresh(siblings)
											entcoll
										}
									),
									active = list(
										entity_groups = function(newval){
											if (private$.entity_groups %>% is.null)
												private$.entity_groups = EntityGroupCollection$new(self$field("entity_group_id"), self$session)
											private$.entity_groups
										},
										entities = function(newval){
											if (private$.entities %>% is.null)
												private$.entities = EntityCollection$new(self$id, self$session)
											private$.entities
										},
										specimen_probes = function(newval){
											if (private$.specimen_probes %>% is.null)
												private$.specimen_probes = SpecimenProbeCollection$new(
													buildStatement(
														table = "specimen_probes",
														columns = "id",
														specimen_probes.entity_id = self$id
													) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
													, self$session
												)

											private$.specimen_probes
										},
										samples = function(newval){
											if (private$.samples %>% is.null)
												private$.samples = SampleCollection$new(
													buildStatement(
														table = "samples",
														joins = list(
															specimen_probes = c("specimen_probes.id", "samples.specimen_probe_id")
														),
														columns = "samples.id AS id",
														specimen_probes.entity_id = self$id
													) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
													, self$session
												)

											private$.samples
										},
										vcf_files = function(newval){
											if (private$.vcf_files %>% is.null)
												private$.vcf_files = VcfFileCollection$new(
													buildStatement(
														table = "vcf_files",
														columns = "vcf_files.id as id",
														joins = list(
															samples = c("samples.vcf_file_id", "vcf_files.id"),
															specimen_probes = c("samples.specimen_probe_id", "specimen_probes.id")
														),
														specimen_probes.entity_id = self$id
													) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
													, self$session
												)

											private$.vcf_files
										}
									),
									private = list()
)
Entity$object_type = "Entity"
Entity$all = function(session, ids = NULL){
	SnupyData$all("entities", ids = ids, session = session, global.lookup = T)
}

#' SnupyEntityTag
#' @export
EntityTag <- R6Class("EntityTag",
										 inherit = SnupyTag,
										 public = list(
										 	initialize = function(id, session) {
										 		private$build("Entity", id, session)
										 	}
										 )
)

