#' Get and EntityGroup object
#'
#' @include snupy_data_collection_classes.R
#'\code{EntityGroup} generates a S6 class
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
#'@return Object of R6 class to use EntityGroups
#'@field name Name of the \code{\link{EntityGroup}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the EntityGroup has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' EntityGroup$new(experiment_id=2)
#' EntityGroup$new(experiment=Experiment$new(2))
#' EntityGroup$new(id=123)
#'@examples
#' eg <- EntityGroup$new(experiment_id=2)
#'@aliases entity_group
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
EntityGroup <- R6Class("EntityGroup",
											 inherit = SnupyData,
											 public = list(
											 	table = "entity_groups",
											 	collection_klass = EntityGroupCollection,
											 	initialize = function(id, session, ...) {
											 		super$initialize(id, "EntityGroup", session, self$table, ...)
											 	},
											 	variation_calls = function(){
											 		(self$entities$each("variation_calls", as.collection = FALSE) %>% rbindlist)[,entity_group_id := self$id]
											 	},
											 	variations = function(r){
											 		(self$entities$each("variations", as.collection = FALSE) %>% c) %>% unlist
											 	},
											 	bafs = function(return.vector = FALSE, combine.baf = T){
											 		ret = (self$entities$each("bafs", as.collection = FALSE, refresh = refresh, return.vector = FALSE, combine.baf = F) %>% rbindlist)[,entity_group_id := self$id]
											 		if (combine.baf){
											 			ret = ret[,list(combine.baf = mean(baf), entity_group_id = entity_group_id), by=variation_id] %>% unique
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
											 	cases = function(){ # return all entities that are cases (have a disease associated and are not shared_control)
													ents = self$entities
													ents.is.control = ents$has_tag(tagvalue = "shared control")
													ents$filter(!ents.is.control)
											 	},
											 	controls = function(){ # return all controls # shared_control tag
											 		ents = self$entities
											 		ents.is.control = ents$has_tag(tagvalue = "shared control")
											 		ents$filter(ents.is.control)
											 	}
											 ),
											 active = list(
											 	entity_groups = function(newval){
											 		if (private$.entity_groups %>% is.null)
											 			private$.entity_groups = EntityGroupCollection$new(self$id, self$session)
											 		private$.entity_groups
											 	},
											 	entities = function(newval){
											 		if (private$.entities %>% is.null)
											 			private$.entities = EntityCollection$new(
											 				buildStatement(
											 					table = "entities",
											 					columns = "id",
											 					entities.entity_group_id = self$id
											 				) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
											 				, self$session
											 			)

											 		private$.entities
											 	},
											 	specimen_probes = function(newval){
											 		if (private$.specimen_probes %>% is.null)
											 			private$.specimen_probes = SpecimenProbeCollection$new(
											 				buildStatement(
											 					table = "specimen_probes",
											 					joins = list(
											 						entities = c("entities.id", "specimen_probes.entity_id")
											 					),
											 					columns = "specimen_probes.id AS id",
											 					entities.entity_group_id = self$id
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
											 						specimen_probes = c("specimen_probes.id", "samples.specimen_probe_id"),
											 						entities = c("specimen_probes.entity_id", "entities.id"),
											 						entity_groups = c("entities.entity_group_id", "entity_groups.id")
											 					),
											 					columns = "samples.id as id",
											 					entity_groups.id = self$id
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
											 						specimen_probes = c("samples.specimen_probe_id", "specimen_probes.id"),
											 						entities = c("specimen_probes.entity_id", "entities.id")
											 					),
											 					entities.entity_group_id = self$id
											 				) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
											 				, self$session
											 			)

											 		private$.vcf_files
											 	}
											 ),
											 private = list()
)
EntityGroup$object_type = "EntityGroup"
EntityGroup$all = function(session, ids = NULL){
	SnupyData$all("entity_groups", ids = ids, session = session, global.lookup = T)
}

#' SnupyEntityGroupTag
#' @export
EntityGroupTag <- R6Class("EntityGroupTag",
													inherit = SnupyTag,
													public = list(
														initialize = function(id, session) {
															private$build("EntityGroup", id, session)
														}
													)
)


