#' Get and VcfFile object
#'
#'@include snupy_data_collection_classes.R
#'\code{VcfFile} generates a S6 class
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
#'@return Object of R6 class to use VcfFiles
#'@field name Name of the \code{\link{VcfFile}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the VcfFile has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{VcfFile}} and \code{\link{VcfFile}}
#'@usage
#' VcfFile$new(experiment_id=2)
#' VcfFile$new(experiment=Experiment$new(2))
#' VcfFile$new(id=123)
#'@examples
#' eg <- VcfFile$new(experiment_id=2)
#'@aliases VcfFile
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
VcfFile <- R6Class("VcfFile",
									 inherit = SnupyData,
									 public = list(
									 	table = "vcf_files",
									 	collection_klass = VcfFileCollection,
									 	initialize = function(id, session, ...) {
									 		columns.to.load = VcfFile$columns(session)
									 		super$initialize(id, "VcfFile", session, "vcf_files", columns.to.load = columns.to.load, ...)
									 	},
									 	download = function(refresh = FALSE, return.content = F){
									 		vcf_file = sprintf("%s/vcf_file_%d.vcf", self$session$CACHEDIR, self$id)
									 		if (!file.exists(vcf_file) || refresh){
									 			url = sprintf("%s/vcf_files/%d/download.txt", self$session$URL, self$id)
									 			headers <- add_headers("accept-encoding"="gzip")
									 			if (!is.null(self$session[["USER"]]))
									 				auth <- authenticate(session[["USER"]], self$session$get_password())
									 			else
									 				auth <- NULL
									 			proxy <- list()
									 			if (!is.null(self$session[["PROXY"]])){
									 				proxy <- use_proxy(self$session[["PROXY"]], self$session[["PROXYPORT"]], self$session[["PROXYUSER"]], self$session$get_proxy_password(), self$session[["PROXYAUTH"]])
									 			}
									 			resp <- GET(url = url,
									 									config = c(headers, auth, proxy, config(ssl_verifypeer = 0L))
									 			) %>%
									 				content(., as="text", type = "text/plain", encoding="UTF-8")
									 			write(resp, file = vcf_file)
									 			if (return.content)
									 				return(resp)
									 		}
									 		if (return.content)
									 			return(read_file(vcf_file))
									 		vcf_file
									 	},
									 	variation_calls = function(..., refresh = FALSE, cache.use = TRUE, full = FALSE){
									 		self$samples$each("variation_calls", refresh = refresh, cache.use = cache.use, cache.overwrite = refresh, full = full, as.collection = F) %>% rbindlist
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
									 	}
									 ),
									 active = list(
									 	entity_groups = function(newval){
									 		if (private$.entity_groups %>% is.null)
									 			private$.entity_groups = EntityGroupCollection$new(
									 				ids = buildStatement(
									 					table = "entities",
									 					columns = "entities.entity_group_id",
									 					joins = list(
									 						specimen_probes = c("entities.id", "specimen_probes.entity_id"),
									 						samples = c("specimen_probes.id", "samples.specimen_probe_id")
									 					),
									 					samples.vcf_file_id = self$id
									 				) %>% snupy.query(., self$session, extract="entity_group_id", cache.use = T) %>% sort %>% unique
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
									 					joins = list(
									 						samples = c("specimen_probes.id", "samples.specimen_probe_id")
									 					),
									 					samples.vcf_file_id = self$id
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
									 					columns = "specimen_probes.id as id",
									 					joins = list(
									 						samples = c("specimen_probes.id", "samples.specimen_probe_id")
									 					),
									 					samples.vcf_file_id = self$id
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
									 					columns = "samples.id as id",
									 					samples.vcf_file_id = self$id
									 				) %>% snupy.query(., self$session, extract="id", cache.use = T) %>% sort %>% unique
									 				, self$session
									 			)

									 		private$.samples
									 	},
									 	vcf_files = function(newval){
									 		if (private$.vcf_files %>% is.null)
									 			private$.vcf_files = VcfFileCollection$new(self$id, self$session)
									 		private$.vcf_files
									 	},
									 	content = function(newval){
									 		if (missing(newval)){
									 			if (private$.content %>% is.null){
									 				private$.content = self$download(refresh, return.content = T)
									 			}
									 		} else {
									 			private$.content <- newval
									 		}
									 		private$.content
									 	}
									 ),
									 private = list(
									 	.content = NULL
									 )
)
VcfFile$object_type = "VcfFile"
VcfFile$columns = function(session){
	snupy.get_columns("vcf_files", session) %>% grep("`content`", ., invert=T, value=T)
}
VcfFile$all = function(session, ids = NULL){
	SnupyData$all("vcf_files", ids = ids, session = session, global.lookup = T, columns = VcfFile$columns(session))
}

#' VcfFileTag
#' @export
VcfFileTag <- R6Class("VcfFileTag",
											inherit = SnupyTag,
											public = list(
												initialize = function(id, session) {
													private$build(id, session, "VcfFile")
												}
											)
)


