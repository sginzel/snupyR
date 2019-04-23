#' Get a SnupyTag object
#'
#'\code{SnupyTag} generates a S6 class
#'
#'@importFrom R6 R6Class
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
SnupyTag <- R6Class("SnupyTag",
										public = list(
											session = NULL,
											id = NULL,
											object_type = NULL,
											record = NULL,
											association_klass = NULL,
											association_collection_klass = NULL,
											associations = NULL,
											klass = NULL,
											#															 	initialize = function(id, session, object_type) {
											initialize = function(id, session, table = NULL, template = NULL, ...) {
												private$build(id, session, "SnupyTag", template = template)
											},
											value = function(){
												ret = self$record$value %>% as.list
												names(ret) = self$record$category
												ret
												#aggregate(self$record$value, by=self$record$category, FUN=c)
											},
											field = function(name){
												self$record[[name]]
											},
											objects = function(){
												if (is.null(self$associations)){
													if (is.null(self$association_ids))
														return(NULL)
													self$associations = self$association_collection_klass$new(self$association_ids, self$session)
												}
												self$associations
											},
											object_ids = function(){
												self$association_ids
											}
										),
										active = list(
											association_ids = function(value){
												if (is.null(private$.association_ids)){
													statement = buildStatement(
														"tag_has_objects",
														columns = "object_id",
														tag_has_objects.tag_id = self$id)
													private$.association_ids = snupy.query(statement, self$session, cache.use = T, extract = "object_id")
												}
												if (missing(value)){
													private$.association_ids
												} else {
													all(value %in% private$.association_ids)
												}
											}
										),
										private = list(
											.association_ids = NULL,
											build = function(id, session, object_type, template = NULL){
												self$klass = SnupyTag
												self$session <- session
												self$id <- id
												if (object_type == "SnupyTag"){
													object_type = buildStatement(
														table = "tags",
														columns = "object_type",
														id = self$id
													) %>% snupy.query(. , session = self$session, extract="object_type", cache.use = T)
												}
												self$object_type = object_type
												if (is.null(template)){
													template = private$get_record()
												}
												self$record = template
												# self$association_ids = private$get_association_ids()
												self$association_klass <- eval(parse(text=self$object_type))
												self$association_collection_klass <- eval(parse(text=sprintf("%sCollection", self$object_type)))
												invisible(self)
											},
											get_record = function(){
												statement = buildStatement(
													"tags",
													columns = snupy.get_columns("tags", self$session),
													limit = 1,
													id = self$id,
													tags.object_type = self$object_type
												)
												record = snupy.query(statement, self$session, cache.use = T)
												if (is.null(record)){
													record = snupy.get_columns("tags", self$session) %>%
														paste(., collapse=",") %>%
														read.csv(text=.) %>% as.data.table
												}
												record
											}
										)
)

SnupyTag$all = function(session, ids = NULL){
	SnupyData$all("tags", ids = ids, session = session, global.lookup = T)
}

#' @export
SnupyTag$find.by.value = function(tag.value, session){
	tagids = buildStatement(
		table = "tags",
		columns = "id",
		value = tag.value
	) %>% snupy.query(., session = session, extract="id")
	if (length(tagids) == 0){
		return(NULL)
	}
	SnupyTagCollection$new(ids = tagids, session = session)
}

#'@export
SnupyTag$find.by.object = function(object, session = object$session){
	tagids = buildStatement(
		table = "tag_has_objects",
		columns = "tag_id",
		object_id = object$id,
		object_type = object$object_type
	) %>%
	snupy.query(.,
							session = session,
							extract = "tag_id",
							cache.use = T
	)
	SnupyTagCollection$new(ids = tagids, session = session)
}

#' SnupyTags
#' @export
SnupyTagCollection <- R6Class("SnupyTagCollection",
															inherit = SnupyDataCollection,
															public = list(
																initialize = function(ids = NULL, session, object = NULL) {
																	if (ids %>% missing){
																		ids = private$find_tag_ids(object)
																	}
																	self$ids = ids
																	self$build(ids = ids, session = session, klass = SnupyTag)
																},
																includes = function(value){
																	self$include(value)
																},
																include = function(value){
																	if (is.null(value))
																		return(FALSE)
																	if (inherits(value, "character")){
																		any(self$values() == value)
																	} else {
																		any(self$ids == value$id)
																	}
																},
																values = function(){
																	self$fields("value")
																},
																categories = function(){
																	self$fields("category")
																},
																subcategories = function(){
																	self$fields("subcategory")
																},
																# returns a list of object collections
																objects = function(){
																	self$each("objects", as.collection = F)
																},
																object_ids = function(){
																	self$each("object_ids", as.collection = F)
																},
																entity_groups = function(){
																	private$hierarchy_objects("entity_groups", EntityGroupCollection)
																},
																entities = function(){
																	private$hierarchy_objects("entities", EntityCollection)
																},
																specimen_probes = function(){
																	private$hierarchy_objects("specimen_probes", SpecimenProbeCollection)
																},
																samples = function(){
																	private$hierarchy_objects("samples", SampleCollection)
																},
																vcf_files = function(){
																	private$hierarchy_objects("vcf_files", VcfFukeCollection)
																},
																finalize = function() {}
															),
															private = list(
																hierarchy_objects = function(funname, collection_klass){
																	num.objects = self$object_ids() %>% unlist %>% length
																	uniq.num.objects = self$object_ids() %>% unlist %>% unique %>% length
																	i = 0
																	cat(sprintf("Retrieving %s \n", funname))
																	pb = txtProgressBar(min = 0, max = num.objects, style = 3)
																	sapply(self$objects(), simplify=F, function(objs){
																		ids = sapply(objs$collection, simplify=F, function(obj){
																			i <<- i + 1
																			setTxtProgressBar(pb, i)
																			obj[[funname]]()$fields("id")
																		}) %>% unlist

																		ids = ids[!is.null(ids)]
																		collection_klass$new(ids = ids, session = self$session)
																	})
																},
																find_tag_ids = function(object){
																	# TODO delete this function
																	buildStatement(
																		table = "tag_has_objects",
																		columns = "tag_id",
																		object_id = object$id,
																		object_type = object$object_type
																	) %>%
																		snupy.query(.,
																								session = self$session,
																								extract = "tag_id"
																		)
																}
															)
)

#'@export Ops.SnupyTag
Ops.SnupyTag <- function(c1, c2){
	op = .Generic[[1]]
	switch(op,
				 `==` = {
				 	attributes(c1$klass)$name == attributes(c2$klass)$name &
				 	c1$id == c2$id
				 },
				 `!=` = {
				 	! (c1 == c2)
				 },
				 stop("undefined operation")
	)
}


#'@export
tags <- function(obj){UseMethod('tags', obj)}
#'@export
has.tag <- function(obj, tag){UseMethod('has.tag', obj)}

#'@export tags.SnupyData
tags.SnupyData <- function(x){
	x$tags
}

#'@export has.tag.SnupyData
has.tag.SnupyData <- function(obj, tag){
	if (is.character(tag)){
		any(obj$tags$each("value") %>% as.list %>% unlist == tag)
	} else {
		#any(obj$tags == tag)
		obj$tags %contains% tag
	}
}

#'@export tags.SnupyDataCollection
tags.SnupyDataCollection <- function(x){
	x$each("tags")
}

#'@export has.tag.SnupyDataCollection
has.tag.SnupyDataCollection <- function(obj, tag){
	sapply(obj$collection, simplify = F, function(coll_obj){
		any(has.tag(coll_obj, tag))
	}) %>% unlist
}

