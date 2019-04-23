#' Get and SpecimenProbe object
#'
#'@include snupy_data_collection_classes.R
#'\code{SpecimenProbe} generates a S6 class
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
#'@return Object of R6 class to use Specimens
#'@field name Name of the \code{\link{SpecimenProbe}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the SpecimenProbe has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{SpecimenProbe}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' SpecimenProbe$new(experiment_id=2)
#' SpecimenProbe$new(experiment=Experiment$new(2))
#' SpecimenProbe$new(id=123)
#'@examples
#' eg <- SpecimenProbe$new(experiment_id=2)
#'@aliases specimen_probe
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
SpecimenProbe <- R6Class("SpecimenProbe",
												 inherit = SnupyData,
												 public = list(
												 	table = "specimen_probes",
												 	collection_klass = SpecimenProbeCollection,
												 	initialize = function(id, session, ...) {
												 		super$initialize(id, "SpecimenProbe", session, "specimen_probes", ...)
												 	},
												 	variation_calls = function(refresh = FALSE, ...){
												 		(self$samples$each("variation_calls", as.collection = FALSE, refresh = refresh) %>% rbindlist)[,specimen_probe_id := self$id]
												 	},
												 	variations = function(refresh = FALSE, ...){
												 		(self$samples$each("variations", as.collection = FALSE, refresh = refresh) %>% c) %>% unlist
												 	},
												 	bafs = function(refresh = FALSE, ..., return.vector = FALSE, combine.baf = T){
												 		ret = (self$samples$each("bafs", as.collection = FALSE, refresh = refresh, return.vector = FALSE, combine.baf = F) %>% rbindlist)[,specimen_probe_id := self$id]
												 		if (combine.baf){
												 			ret = ret[,list(combine.baf = mean(baf), specimen_probe_id = specimen_probe_id), by=variation_id] %>% unique
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
												 	entity_groups = function(newval){

												 		if (private$.entity_groups %>% is.null)
												 			private$.entity_groups = EntityGroupCollection$new(
												 				buildStatement(
												 					table = "entities",
												 					columns = "entity_group_id as id",
												 					joins = list(
												 						specimen_probes = c("entities.id", "specimen_probes.entity_id")
												 					),
												 					specimen_probes.id = self$id
												 				) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
												 				, self$session
												 			)

												 		private$.entity_groups
												 	},
												 	entities = function(newval){

												 		if (private$.entities %>% is.null)
												 			private$.entities = EntityCollection$new(
												 				buildStatement(
												 					table = "specimen_probes",
												 					columns = "specimen_probes.entity_id as id",
												 					id = self$id
												 				) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
												 				, self$session)

												 		private$.entities
												 	},
												 	specimen_probes = function(newval){
												 		if (private$.specimen_probes %>% is.null)
												 			private$.specimen_probes = SpecimenProbe$new(self$id, self$session)
												 		private$.specimen_probes
												 	},
												 	samples = function(newval){

												 		if (private$.samples %>% is.null)
												 			private$.samples = SampleCollection$new(
												 				snupy.query(
												 					buildStatement(
												 						table = "samples",
												 						columns = "id",
												 						specimen_probe_id = self$id
												 					)
												 					, self$session, extract = "id", cache.use = T) %>% sort %>% unique
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
												 						samples = c("samples.vcf_file_id", "vcf_files.id")
												 					),
												 					samples.specimen_probe_id = self$id
												 				) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
												 				, self$session
												 			)

												 		private$.vcf_files
												 	}
												 ),
												 private = list()
)
SpecimenProbe$object_type = "SpecimenProbe"
SpecimenProbe$all = function(session, ids = NULL){
	SnupyData$all("specimen_probes", ids = ids, session = session, global.lookup = T)
}
#' SnupySpecimenTag
#' @export
SpecimenProbeTag <- R6Class("SpecimenProbeTag",
														inherit = SnupyTag,
														public = list(
															initialize = function(id, session) {
																private$build(id, session, "SpecimenProbe")
															}
														)
)


