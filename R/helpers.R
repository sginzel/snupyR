# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

anyNA <- function(x){
  is.na(x) %>% any
}

snupy.cache <- function(FUN, cache.session, ..., cache.filename = NULL, cache.force = F){
  stopifnot(is.function.snupy(FUN))
  if (is.null(cache.filename)){
    filename = sprintf("%s/%s_%s.RData", cache.session[["CACHEDIR"]], as.character(substitute(FUN)), digest(c(..., cache.session[["DBHOST"]], cache.session[["DBNAME"]])))
  } else {
    filename = sprintf("%s/%s_%s.RData", cache.session[["CACHEDIR"]], as.character(substitute(FUN)), cache.filename)
  }
  if ( (!file.exists(filename)) || (cache.force) ){
    if (!(is.null(cache.session[["DEBUG"]])) && (cache.session[["DEBUG"]] == TRUE)) cat("caching to ", filename, "...")
    cached = FUN(...)
    if (!(is.null(cache.session[["DEBUG"]])) && (cache.session[["DEBUG"]] == TRUE)) cat("DONE\n")
    save(cached, file=filename)
  } else {
    if (!(is.null(cache.session[["DEBUG"]])) && (cache.session[["DEBUG"]] == TRUE)) cat("use cache from ", filename, "...")
    load(filename)
    if (!(is.null(cache.session[["DEBUG"]])) && (cache.session[["DEBUG"]] == TRUE)) cat("DONE\n")
  }
  return(cached)
}

#'@export
snupy.get_columns <- function(table, session, quote.char = "`"){
	snupy.query(sprintf("DESCRIBE %s", table), session, cache.use = TRUE)$Field %>%
		paste(quote.char, table, quote.char, ".", quote.char, ., quote.char, sep="")
}

snupy.get_column_alias <- function(table, session, quote.char = "`"){
	snupy.query(sprintf("DESCRIBE %s", table), session, cache.use = TRUE)$Field %>%
		paste(quote.char, table, quote.char, ".", quote.char, ., quote.char, "AS", quote.char, table, ".", ., quote.char, sep="")
}

chunk <- function(x,n)
{
	f <- sort(rep(1:(trunc(length(x)/n)+1),n))[1:length(x)]
	return(split(x,f))
}
