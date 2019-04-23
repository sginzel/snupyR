# make some nice S3 dispatches
#' @include snupy_data_collection.R
#'@export as.list.SnupyDataCollection
as.list.SnupyDataCollection <- function(sdc, ...){as.list(sdc$collection, ...)}
#'@export as.vector.SnupyDataCollection
as.vector.SnupyDataCollection <- function(x, mode){as.vector(as.list(x))}

#'@export
intersect.collections <- function(s1, s2){UseMethod('intersect.collections', s1)}
#'@export
union.collections <- function(s1, s2){UseMethod('union.collections', s1)}
#'@export
setdiff.collections <- function(s1, s2){UseMethod('setdiff.collections', s1)}

#'@export %contains%
`%contains%` <- function(x, y){UseMethod('%contains%', x)}
#'@export %partof%
`%partof%` <- function(x, y){UseMethod('%partof%', x)}

#'@export Ops.SnupyDataCollection
Ops.SnupyDataCollection <- function(c1, c2){
	op = .Generic[[1]]
	switch(op,
				 `==` = {
				 	attributes(c1$klass)$name == attributes(c2$klass)$name &
				 		all(c1$field("id") %>% unlist %in% c2$field("id")  %>% unlist) &
				 		all(c2$field("id") %>% unlist %in% c1$field("id")  %>% unlist)
				 },
				 `!=` = {
				 	! (x == y)
				 },
				 `-` = {
				 	setdiff.collections(c1, c2)
				 },
				 `+` = {
				 	newcoll = c1$clone()
				 	newcoll$refresh(c(c1$collection, c2$collection))
				 	newcoll
				 },
				 `&` = {
				 	intersect.collections(c1, c2)
				 },
				 `|` = {
				 	union.collections(c1, c2)
				 },
				 stop("undefined operation")
	)
}
Ops.SnupyTagCollection <- Ops.SnupyDataCollection
Ops.ExperimentCollection <- Ops.SnupyDataCollection
Ops.EntityGroupCollection <- Ops.SnupyDataCollection
Ops.EntityCollection <- Ops.SnupyDataCollection
Ops.SpecimenProbeCollection <- Ops.SnupyDataCollection
Ops.SampleCollection <- Ops.SnupyDataCollection
Ops.VcfFileCollection <- Ops.SnupyDataCollection



#'@export intersect.SnupyDataCollection
intersect.SnupyDataCollection <- function(x,y){
	x$filter(x$fields("id") %in% y$fields("id"))
}

#'@export unique.SnupyDataCollection
unique.SnupyDataCollection <- function(x){
	x$filter(!duplicated(x$ids))
}

#'@export
intersect.collections.SnupyDataCollection <- function(x,y){
	if (attributes(x$klass)$name != attributes(y$klass)$name)
		stop("Collection intersection requires the same klass")
	x$filter(x$fields("id") %in% y$fields("id"))
}

#'@export
union.collections.SnupyDataCollection <- function(x,y){
	if (attributes(x$klass)$name != attributes(y$klass)$name)
		stop("Collection union requires the same klass")
	ret = x$clone()
	ret$collection = c(ret$collection, setdiff.collections(y, x)$collection)
	ret = SnupyDataCollection$new(NULL, x$session, x$klass, objects = ret$collection)
	# ret$ids = sapply(ret$collection, simplify=F, function(x){x$id}) %>% unlist
	ret
}

#'@export
setdiff.collections.SnupyDataCollection <- function(x,y){
	if (attributes(x$klass)$name != attributes(y$klass)$name)
		stop("Collection setdiff requires the same klass")
	ret = x$filter(!x$fields("id") %in% y$fields("id"))
	ret
}

#' @export
`%contains%.SnupyDataCollection` <- function(x, y){
	if (!attributes(x$klass)$name == attributes(y$klass)$name)
		stop("Klass of collection has to be the same as of the object")
	any(sapply(x$collection, function(x1){x1 == y}))
}

#' @export
`%partof%.SnupyData` <- function(x, y){
	y %contains% x
}
