#' Get a GenePanel object
#'
#'\code{GenePanel} generates a S6 class
#'
#'@importFrom R6 R6Class
#'@param filepath a path to a .r file that contains the config
#'@param connect logical value whether ssh tunnel should be connected or not.
#'@details The file containig the configuration should set the following variables\cr
#'\describe{
#'	\item{name}{logical value if debug information should be shown}
#'	\item{tags}{tags linked to this entity group}
#'}
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
GenePanel <- R6Class("GenePanel",
										 public = list(
										 	id = NULL,
										 	session = NULL,
										 	initialize = function(id, session, template = NULL) {
										 		self$id <- id
										 		if (self$id %>% is.null)
										 			if (!template %>% is.null)
										 				self$id = template$id
										 		self$session <- session
										 		private$load()
										 		stopifnot(!is.null(self$session))
										 		invisible(self)
										 	},
										 	field = function(name){
										 		private$.record[[name]]
										 	},
										 	includes = function(gene){
										 		any(gene %in% self$genes)
										 	},
										 	includes.all = function(genes){
										 		all(genes %in% self$genes)
										 	}
										 ),
										 active = list(
										 	genes = function(newval){
										 		if (!missing(newval))
										 			private$.genes = newval
										 		private$.genes
										 	},
										 	record = function(newval){
										 		if (!missing(newval))
										 			private$.record = newval
										 		private$.record
										 	},
										 	items = function(newval){
										 		if (!missing(newval))
										 			private$.items = newval
										 		private$.items
										 	}
										 ),
										 private = list(
										 	.genes = NULL,
										 	.record = NULL,
										 	.items = NULL,
										 	load = function(){
												private$.record = snupy.request(sprintf("generic_lists/%d", self$id), format="json", session = self$session, http.method = GET)
												private$.items = private$.record[["items"]] %>% sapply(., simplify=F, . %>% unlist %>% as.list)
												private$.genes = private$.record[["genes"]]
												private$.record[["items"]] = NULL
												private$.record[["genes"]] = NULL
												private$.record = private$.record %>% as.data.table
												setnames(private$.record, "id", "generic_list_id")
										 	}
										 )
)

GenePanel$all <- function(session, ids = NULL, ...){
	SnupyData$all(table = "generic_lists", ids = ids, session = session, ...)
}
GenePanel$find.by.name <- function(panel.name, session){
	GenePanel$new(subset(GenePanel$all(session), name == panel.name)$id, session)
}

#'@export as.list.GenePanel
as.list.GenePanel <- function(obj, ...){
	ret = as.list(obj$record, ...)
	ret$genes = obj$genes
	ret
}

#'@export as.vector.GenePanel
as.vector.GenePanel <- function(obj, mode){as.vector(as.list(obj))}

#'@export as.data.table.GenePanel
as.data.table.GenePanel <- function(obj, mode){
	panel = obj$record #sapply(obj$record, simplify=F, as.data.table) %>% rbindlist(., fill=T) %>% unique
	items = sapply(obj$items, simplify=F, as.data.table) %>% rbindlist(., fill=T) %>% unique
	items$generic_list_id = as.integer(items$generic_list_id)
	items$created_at=NULL
	items$updated_at=NULL
	panel[items, on="generic_list_id", allow.cartesian=T]
}


#'@export Ops.GenePanel
Ops.GenePanel <- function(c1, c2){
	op = .Generic[[1]]
	switch(op,
				 `==` = {
				 	all(c1$genes %in% c2$genes) &
				 	all(c2$genes %in% c1$genes)
				 },
				 `!=` = {
				 	! (x == y)
				 },
				 `+` = {
				 	ret = c1$clone()
				 	ret$genes <- c(c1$genes, c2$genes) %>% unique
				 	ret$items <- c(c1$items, c2$items) %>% unique
				 	ret$record <- list(c1$record, c2$record) %>% unique
				 	ret
				 },
				 stop("undefined operation")
	)
}

