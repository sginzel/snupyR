#' Experiment
#' @export
ExperimentCollection <- R6Class("ExperimentCollection",
														 inherit = SnupyDataCollection,
														 public = list(
														 	initialize = function(ids, session) {
														 		super$build(ids, session, VcfFile)
														 	},
														 	finalize = function() {}
														 ),
														 private = list()
)
#' EntityGroupsCollection
#' @export
EntityGroupCollection <- R6Class("EntityGroupCollection",
																 inherit = SnupyDataCollection,
																 public = list(
																 	initialize = function(ids = NULL, session, project = NULL) {
																 		if (is.null(ids)){
																 			if (!is.null(project)){
																 				cat ("retrieving entitiy groups for project ", project, "\n", sep="")
																 				if (is.character(project)){
																 					cat ("id for for project ", project, sep="")
																 					project = buildStatement(
																 						table = "experiments",
																 						columns = "id",
																 						name = project
																 					) %>% snupy.query(., session, extract="id") %>% unique
																 					cat (" => ", project, "\n", sep="")
																 				}
																 				cat ("Getting EntityGroup ids for ", project, sep="")
																 				ids = buildStatement(
																 					table = "experiment_has_entity_groups",
																 					columns = "entity_group_id",
																 					experiment_id = project
																 				) %>% snupy.query(., session, extract="entity_group_id") %>% unique
																 				cat ("=> #", length(ids), "\n", sep="")
																 			}
																 		}
																 		super$build(ids, session, EntityGroup)
																 	},
																 	finalize = function() {}
																 ),
																 private = list()
)

#' EntitysCollection
#' @export
EntityCollection <- R6Class("EntityCollection",
														inherit = SnupyDataCollection,
														public = list(
															initialize = function(ids, session) {
																super$build(ids, session, Entity)
															},
															finalize = function() {}
														),
														private = list()
)
#' SpecimensCollection
#' @export
SpecimenProbeCollection <- R6Class("SpecimenProbeCollection",
																	 inherit = SnupyDataCollection,
																	 public = list(
																	 	initialize = function(ids, session) {
																	 		super$build(ids = ids, session = session, klass = SpecimenProbe)
																	 	},
																	 	finalize = function() {}
																	 ),
																	 private = list()
)
#' SamplesCollection
#' @export
SampleCollection <- R6Class("SampleCollection",
														inherit = SnupyDataCollection,
														public = list(
															initialize = function(ids, session) {
																super$build(ids, session, Sample)
															},
															finalize = function() {}
														),
														private = list()
)
#' VcfFilesCollection
#' @export
VcfFileCollection <- R6Class("VcfFileCollection",
														 inherit = SnupyDataCollection,
														 public = list(
														 	initialize = function(ids, session) {
														 		super$build(ids, session, VcfFile)
														 	},
														 	finalize = function() {}
														 ),
														 private = list()
)
#' GenePanelCollection
#' @export
GenePanelCollection <- R6Class("GenePanelCollection",
															 inherit = SnupyDataCollection,
															 public = list(
															 	initialize = function(ids, session) {
															 		super$build(ids, session, GenePanel)
															 	},
															 	finalize = function() {},
															 	includes = function(genes){
															 		any(unlist(self$each("genes")) == genes)
															 	},
															 	includes.all = function(genes){
															 		all(genes %in% unlist(self$each("genes")))
															 	},
															 	tags = function(tag_level = c("self", "entity_groups", "entities", "specimen_probes", "samples", "vcf_files")){},
															 	has_tag = function(tagvalue = NULL, tag_level = c("self", "entity_groups", "entities", "specimen_probes", "samples", "vcf_files")){},
															 	variation.matrix = function(attribute = c("present", "baf"), min.dp = 100, col.row.name = "id"){}
															 ),
															 private = list()
)
