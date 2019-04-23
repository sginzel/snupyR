#' Get and Experiment object
#'
#' @include snupy_data_collection_classes.R
#'\code{Experiment} generates a S6 class
#'
#'@importFrom R6 R6Class
#'@include snupy_data.R
#'@param filepath a path to a .r file that contains the config
#'@param connect logical value whether ssh tunnel should be connected or not.
#'@details Experiments contain full datasets, which means unlike the other elements of the
#' sample hierarchy the relations to entity groups, entities and specimen rely soley on the
#' association between the experiment and the entity group and the assoctied samples.
#' If any link is missing the samples cannot be queried. It is also possible to filter the
#' entity groups, entities, specimens and samples of an experiment to reduce the number of relvant samples.
#'\describe{
#'	\item{name}{logical value if debug information should be shown}
#'	\item{tags}{tags linked to this entity group}
#'}
#'@section Methods:
#'@return Object of R6 class to use Experiments
#'@field name Name of the \code{\link{Experiment}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the Experiment has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' Experiment$new(experiment_id=2)
#' Experiment$new(experiment=Experiment$new(2))
#' Experiment$new(id=123)
#'@examples
#' eg <- Experiment$new(experiment_id=2)
#'@aliases entity_group
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
Experiment <- R6Class("Experiment",
											 inherit = SnupyData,
											 public = list(
											 	table = "experiments",
											 	collection_klass = ExperimentCollection,
											 	initialize = function(id_or_name, session, ...) {
											 		expid = buildStatement(
											 			table = "experiments",
											 			columns = "id",
											 			id_or_name = list(raw=sprintf("id = '%s' OR name = '%s' OR title = '%s'", as.character(id_or_name), as.character(id_or_name), as.character(id_or_name)))
											 		) %>% snupy.query(., session = session, cache.use = F, extract="id")
											 		super$initialize(expid[[1]], "Experiment", session = session, self$table, ...)
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
											 	}
											 ),
											 active = list(
											 	experiments = function(newval){
											 		if (private$.experiments %>% is.null)
											 			private$.experiments = ExperimentCollection$new(self$id, self$session)
											 		private$.experiments
											 	},
											 	# experiments do not use database association, but rely rather on _complete_ datasets
											 	entity_groups = function(newval){
											 		if (missing(newval)){
											 			if (private$.entity_groups %>% is.null)
											 				private$.entity_groups = EntityGroupCollection$new(
											 					buildStatement(
											 						table = "experiment_has_entity_groups",
											 						columns = "experiment_has_entity_groups.entity_group_id as id",
											 						experiment_has_entity_groups.experiment_id = self$id %>% unlist %>% unique
											 					) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
											 					, self$session
											 				)
											 		} else {
											 			private$.entity_groups = newval
											 		}
											 		private$.entity_groups
											 	},
											 	entities = function(newval){
											 		if (missing(newval)){
											 			if (private$.entities %>% is.null)
											 				private$.entities = EntityCollection$new(
											 					buildStatement(
											 						table = "entities",
											 						columns = "entities.id as id",
											 						entity_group_id = self$entity_groups$ids %>% unlist %>% unique
											 					) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
											 					, self$session
											 				)
											 		} else {
											 			private$.entities = newval
											 		}
											 		private$.entities
											 	},
											 	specimen_probes = function(newval){
											 		if (missing(newval)){
											 			if (private$.specimen_probes %>% is.null)
											 				private$.specimen_probes = SpecimenProbeCollection$new(
											 					buildStatement(
											 						table = "specimen_probes",
											 						columns = "specimen_probes.id as id",
											 						specimen_probes.entity_id = self$entities$ids %>% unlist %>% unique
											 					) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
											 					, self$session
											 				)
											 		} else {
											 			private$.specimen_probes = newval
											 		}
											 		private$.specimen_probes
											 	},
											 	samples = function(newval){
											 		if (missing(newval)){
											 			if (private$.samples %>% is.null)
											 				private$.samples = SampleCollection$new(
											 					buildStatement(
											 						table = "samples",
											 						columns = "samples.id as id",
											 						samples.specimen_probe_id = self$specimen_probes$ids %>% unlist %>% unique
											 					) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
											 					, self$session
											 				)
											 		} else {
											 			private$.samples = newval
											 		}
											 		private$.samples
											 	},
											 	vcf_files = function(newval){
											 		if (missing(newval)){
											 			if (private$.vcf_files %>% is.null)
											 				private$.vcf_files = VcfFileCollection$new(self$samples$each("vcf_files") %>% sapply(., .%>% .$ids) %>% unlist, self$session)
											 		} else {
											 			private$.vcf_files = newval
											 		}
											 		private$.vcf_files
											 	}
											 ),
											 private = list()
)
Experiment$object_type = "Experiment"
Experiment$all = function(session, ids = NULL){
	SnupyData$all("experiments", ids = ids, session = session, global.lookup = T)
}


