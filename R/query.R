#' Sends a SQL query to the SNUPY API
#'
#'\code{snupy.query} sends a SQL query.
#'
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'
#'@param statement SQL query to execute.
#'@param session session variable as returned by \code{\link{snupy.createSession}}
#'@param primary.key can be character vector, or method. If it is a character vector
#' the key of the data.table object will be set to primary.key. If a method is passed
#' it should accept each record of the response as a parameter and return a key to be used.
#' When 'md5' is used as a parameter the method will calculate the md5 sum of each record
#' and use it as a key. Requires openssl package.
#'@param method use either wget command or curllib
#'@param format by default a data.table object is returned, but tibbles can be used as well.
#'@details currently using curl also enables you to use a proxy with authentification.
#'@return Will return the result of the query as a data.table (default) or tibble.
#'@seealso \code{\link{snupy.createSession}}
#'@aliases query
#'@family snupy
#'@export
snupy.query <- function(statement, session, primary.key = NULL, method=c("curl", "wget"), format=c("data.table", "tibble"), extract = NULL, extract.name = NULL, cache.use = FALSE, cache.overwrite = FALSE, http.format = "csv"){
	cache.name = NULL
	if (is.character(cache.use)){
		cache.name = cache.use
		cache.use = TRUE
	}
	if (cache.use){
		if (!(is.null(session[["SHOWSQL"]])) && (session[["SHOWSQL"]] == TRUE) && (cache.use && !cache.overwrite))
			cat("(CACHED)")
		session$CACHE$call(snupy.doquery, statement = statement, session = session, primary.key = primary.key,
											 method=method, format=format, extract = extract, extract.name = extract.name, memoize.force = cache.overwrite, memoize.name = cache.name, http.format = http.format)
	} else {
		snupy.doquery(statement = statement, session = session, primary.key = primary.key,
											 method=method, format=format, extract = extract, extract.name = extract.name, http.format = http.format)
	}

}
# TODO consolidate snupy.do query with snupy.request - both basically do the same thing...
snupy.doquery <- function(statement, session, primary.key = FALSE, method=c("curl", "wget"), format=c("data.table", "tibble"), extract = NULL, extract.name = NULL, http.format = "csv"){
	if (!(is.null(session[["SHOWSQL"]])) && (session[["SHOWSQL"]] == TRUE)){
		cat("QUERY", substr(statement, 1, 1024), "\n")
	}
	stopifnot(!is.null(session))
	if (statement %>% is.null)
		return(NULL)

	url <- sprintf("%s/api", session[["URL"]])

	if (format[[1]] == "tibble"){
		format.method <- . %>% as.tibble(., col_types = cols()) # function(x) {x %>% read_delim(., delim="\t", col_types = cols())} #function(x) {x %>% fread(., sep = "\t", quote = "\"", header = T, stringsAsFactors = F) %>% as.tibble(., col_types = cols())}
	} else {
		format.method <- as.data.table# function(x) {x %>% fread(., sep = "\t", quote = "\"", header = T, stringsAsFactors = F)}#as.data.table
	}
	# instantly return and skip all conversion to a data.table or tibble
	if (http.format == "json")
		format.method = return

	if (method[[1]] == "wget"){
		postdata = sprintf("query=%s&format=%s", statement, http.format) %>% URLencode
		cmd <- sprintf("wget -qO- --no-check-certificate --post-data='%s' --http-user=%s --http-password=%s --header='accept-encoding: gzip' '%s' | gunzip", postdata, session$USER, session$get_password(), url)
		resp <- fread(cmd, stringsAsFactors = FALSE) %>%
			format.method
	} else {
		headers <- add_headers("accept-encoding"="gzip")
		if (!(is.null(session[["USER"]])))
			auth <- authenticate(session[["USER"]], session$get_password())
		else
			auth <- NULL
		proxy <- list()
		if (!is.null(session[["PROXY"]])){
			proxy <- use_proxy(session[["PROXY"]], session[["PROXYPORT"]], session[["PROXYUSER"]], session$get_proxy_password(), session[["PROXYAUTH"]])
		}
		resp <- POST(url = url,
								 c(headers, auth, proxy, config(ssl_verifypeer = 0L)),
								 encode = "json",
								 body=list(
								 	query=statement,
								 	format=http.format
								 )) %>%
								 {
								 	if (status_code(.) != 200){
								 		{warning(sprintf("Status code was %d(%s)", status_code(.), http_status(.)));print(.); NULL}
								 	} else {
								 		if (http.format == "csv"){
								 			mime.type = "text/tab-separated-values"
								 			content(., as="parsed", type = mime.type, encoding="UTF-8", col_types = cols()) %>%
								 				format.method
								 		} else if (http.format == "json") {
								 			mime.type = "application/json"
								 			content(., as="parsed", type = mime.type, encoding="UTF-8", col_types = cols()) %>%
								 				format.method
								 		} else {
								 			stop("http.format is invalid")
								 		}
								 	}
								 }
	}

	# set key
	if (any(class(resp) == "data.table")){
		if (!is.null(primary.key)){
			if (is.function(primary.key)){
				resp$"_key" = apply(resp, 1, primary.key)
				setkeyv(x=resp, cols="_key")
			} else {
				if (!primary.key == FALSE){
					if (class(primary.key) == "character"){
						if (primary.key == "md5"){
							resp$"_key" = apply(resp, 1, function(x){paste0(sort(x), collapse="") %>% digest(.)})
							setkeyv(x=resp, cols="_key")
						} else {
							if (all(primary.key %in% names(resp))){
								setkeyv(x=resp, cols=primary.key)
							}
						}
					} else {

					}
				}
			}
		}
	}
	if (!is.null(extract)){
		if (!is.null(extract.name)){
			extract.name = make.unique(resp[[extract.name]] %>% as.character)
		}
		if (length(extract) == 1){
			resp = resp[[extract]]
		} else {
			resp = resp[,extract]
		}
		names(resp) = extract.name
	}
	return(resp)
}

snupy.request.parse_result <- function(result){
	ret <- tryCatch({
		result %>% fread(., sep = "\t", quote = "'", header = T, stringsAsFactors = F)
	}	,
	error = function(e){
		e
	})
	if (!is.data.table(ret)){
		ret = data.table()
	}
	ret
}

#'@export
snupy.request <- function(resource, params = NULL, format = c("csv", "json", "yaml", "text", "html"), session, cache.use = T, http.method = POST){

	url <- sprintf("%s/%s.%s", session[["URL"]], resource, format[[1]])
	headers <- add_headers("accept-encoding"="gzip", Connection = "keep-alive", "Keep-Alive" = "timeout=60, max=240")
	if (!is.null(session[["USER"]]))
		# auth <- authenticate(session[["USER"]], session[["PASS"]])
		auth <- authenticate(session[["USER"]], session$get_password())
	else
		auth <- NULL
	proxy <- list()
	if (!is.null(session[["PROXY"]])){
		# proxy <- use_proxy(session[["PROXY"]], session[["PROXYPORT"]], session[["PROXYUSER"]], session[["PROXYPASS"]], session[["PROXYAUTH"]])
		proxy <- use_proxy(session[["PROXY"]], session[["PROXYPORT"]], session[["PROXYUSER"]], session$get_proxy_password(), session[["PROXYAUTH"]])
	}
	# myparams = params
	# myparams[["format"]] = format[[1]]
	if ((!cache.use) & (!session$CACHE %>% is.null))
		resp <- snupy.parse_response(http.method, url = url,
																 c(headers, auth, proxy, config(ssl_verifypeer = 0L), timeout(60*60*4)), # 4 hour timeout sounds good
																 body=params,
																 encode = "json",
																 http.format = format[[1]],
																 format.method = switch(format[[1]],
																 												csv = snupy.request.parse_result,
																 												json = return,
																 											  yaml = return,
																 											 	text = return
																 									)
																 )
	else
		resp <- session$CACHE$call(
			snupy.parse_response,
			http.method, url = url,
			c(headers, auth, proxy, config(ssl_verifypeer = 0L)),
			body=params,
			encode = "json", # parameter encoding
			http.format = format[[1]],
			format.method = switch(format[[1]],
														 #csv = as.data.table,
														 csv = snupy.request.parse_result,
														 json = return,
														 yaml = return,
														 text = return
			)
		)

	if (length(resp) == 1 && (format == "json" || format == "yaml"))
		resp = resp[[1]]
	resp

}

snupy.parse_response <- function(FUN, ..., http.format, format.method = return){
	FUN(...) %>%
	{
		if (status_code(.) != 200){
			{warning(sprintf("Status code was %d(%s)", status_code(.), http_status(.)));print(.); NULL}
		} else {
			if (http.format == "csv"){
				mime.type = "text/tab-separated-values"
				#content(., as="parsed", type = mime.type, encoding="UTF-8", col_types = cols(), quote = "'") %>% # unfortunately we loose the nice streaming feature, but its more reliable to parse it ourselves
				content(., as="text", type = mime.type, encoding="UTF-8") %>%
					format.method
			} else if (http.format == "json") {
				mime.type = "application/json"
				content(., as="parsed", type = mime.type, encoding="UTF-8") %>%
					format.method
			} else if (http.format == "html") {
				mime.type = "text/html"
				content(., as="parsed", type = mime.type, encoding="UTF-8") %>%
					format.method
			} else if (http.format == "text") {
				mime.type = "text/plain"
				content(., as="text", type = mime.type, encoding="UTF-8") %>%
					format.method
			} else {
				stop("format is invalid")
			}
		}
	}
}


## This function takes SQL conditions as "..." part of the parameters
## The parameters are translated as follows
###    VAR = XXX ==> var = XXX - depending on type XXX will be surrounded by quotes or not
###    VAR = c(XX, YY) ==> var IN (XX, YY)
###    VAR = list(between=c(from, to)) ==> VAR BETWEEN from AND to
###    VAR = list(gt = X, lt = Y, eq=Z, GE = XX, LE = YY) ==> VAR > X OR VAR < Y OR VAR = Z OR VAR >= XX OR VAR <= YY
###    VAR = list(raw="RAW SQL CONDITION") ==> RAW SQL CONDITION
### RETURNS A STRING OF - AND - CONCATENATED CONDITIONS
get_sql_where <- function(condition.list, ...){
  conditions = c()
  if (missing(condition.list)){
  	params = list(...)
  } else {
  	params = condition.list
  }
  params.values = params
  params.names = names(params)

  ## first slot is the function name
  # for (i in 2:length(params.values)){
  for (i in 1:length(params.values)){
    conditions[i] = switch(class(params.values[[i]]),
                           NULL = {
                             sprintf("%s IS NULL", params.names[[i]])
                           },
                           numeric = {
                             if (length(params.values[[i]]) == 1)
                               sprintf("%s = %d", params.names[[i]], params.values[[i]])
                             else
                               sprintf("%s IN (%s)", params.names[[i]], paste(params.values[[i]], sep=",", collapse=","))
                           },
                           integer = {
                             if (length(params.values[[i]]) == 1)
                               sprintf("%s = %d", params.names[[i]], params.values[[i]])
                             else
                               sprintf("%s IN (%s)", params.names[[i]], paste(params.values[[i]], sep=",", collapse=","))
                           },
                           character = {
                             if (length(params.values[[i]]) == 1)
                               sprintf("%s = '%s'", params.names[[i]], params.values[[i]])
                             else
                               sprintf("%s IN (%s)", params.names[[i]], paste("'", params.values[[i]], "'", sep="", collapse=","))
                           },
                           list = {
                             ret = NULL#"1 = 0"
                             if (length(names(params.values[[i]])) > 0){
	                             if (all(names(params.values[[i]]) == c("between"))){
	                               stopifnot(length(params.values[[i]]) == 2)
	                               ret = sprintf("%s BETWEEN %d AND %d", params.names[[i]], params.values[[i]][1], params.values[[i]][2])
	                             }
	                             if (all(names(params.values[[i]]) %in% c("gt", "eq", "lt", "ge", "le"))){
	                               ret = c()
	                               for (j in 1:length(params.values[[i]])){
	                                 if (names(params.values[[i]])[[j]] == "gt") ret[j] = sprintf("%s > %d" , params.names[[i]], params.values[[i]][[j]])
	                                 if (names(params.values[[i]])[[j]] == "lt") ret[j] = sprintf("%s < %d" , params.names[[i]], params.values[[i]][[j]])
	                                 if (names(params.values[[i]])[[j]] == "ge") ret[j] = sprintf("%s >= %d", params.names[[i]], params.values[[i]][[j]])
	                                 if (names(params.values[[i]])[[j]] == "le") ret[j] = sprintf("%s <= %d", params.names[[i]], params.values[[i]][[j]])
	                                 if (names(params.values[[i]])[[j]] == "eq") ret[j] = sprintf("%s = %d" , params.names[[i]], params.values[[i]][[j]])
	                               }
	                               ret = sprintf("(%s)", paste(ret, sep=" OR ", collapse=" OR "))
	                             }
	                             if (all(names(params.values[[i]]) == c("raw"))){
	                               ret = params.values[[i]]
	                             }
                             } else if (length(params.values[[i]]) > 0){
                             	ret = params.values[[i]]
                             }
                             ret
                           }
    )
  }
  conditions = conditions[!is.null(conditions)]
  if (any(is.na(conditions))){
  	stop(sprintf("%s is not valid where condition", params))
  }
  if (length(conditions) > 0)
  	return(paste("(", conditions, ")", sep="", collapse=" AND "))
  return(NULL)
}

getSelectColumns <- function(..., session){
  tables = unique(unlist(as.list(...)))
  columns = sapply(tables, simplify=F, function(t){
    cols = snupy.query(sprintf("DESCRIBE %s", t), session)$Field
    cols = cols[!cols %in% c("created_at", "updated_at")]
    paste0(
      sprintf("%s.", t),
      cols
    )
  })
  as.vector(unlist(
    sapply(columns, simplify=F, function(cn){
      sprintf("%s AS `%s`", cn, cn)
    })
  ))
}

get_sql_join <- function(joins){
	join_statement = sapply(names(joins), simplify=F, function(jointbl){
		ret = NULL
		if (length(joins[[jointbl]]) >= 2){
			ret = sprintf("INNER JOIN %s ON (%s = %s)", jointbl, joins[[jointbl]][[1]], joins[[jointbl]][[2]])
			if (length(joins[[jointbl]]) > 2)
				warning("More than two join conditions given. Only the first 2 were used.")
		} else if (length(joins[[jointbl]]) == 1){
			ret = sprintf("INNER JOIN %s ON (%s.id = %s)", jointbl, jointbl, joins[[jointbl]][[1]])
		} else {
			warning
		}
		ret
	})
	join_statement = paste(join_statement, sep="", collapse = "\n")
	join_statement
}

## JOINS is a list in the form of
# join_table => c(column_in_jointable, column_in_primary_table)
#' Buid A SQL statement
#'@export
buildStatement <- function(table, columns = "*", joins = list(), limit = "", group.by = "", having = "", order.by = "", ...){
  ## prepare where clause
  if (length(list(...)) > 0){
    where_statement = get_sql_where(...)
  } else {
    where_statement = c()
  }
  # prepare join statement
	join_statement = get_sql_join(joins)
  all.tables = unique(c(table, names(joins)))

  # prepare columns and names
  col.has.name = names(columns) != ""
  if (any(col.has.name)){
    newcolumns = c()
    for (i in 1:length(col.has.name)){
      if (col.has.name[[i]]){
        col = sprintf("%s AS `%s`", names(columns)[[i]], columns[[names(columns)[[i]]]])
      } else {
        col = columns[[i]]
      }
      newcolumns = c(newcolumns, col)
    }
    columns = newcolumns
  }
  # create statement
  statement = sprintf("
		SELECT %s
		FROM %s", paste0(columns, collapse=","), table)
  if (length(joins) > 0 ){ statement = sprintf("%s\n%s", statement, join_statement) }
  if (length(where_statement) > 0 ){ statement = sprintf("%s\nWHERE %s", statement, where_statement) }
  if (!is.null(group.by) && nchar(group.by[[1]]) > 0) { statement = sprintf("%s\nGROUP BY %s", statement, group.by[[1]]) }
  if (!is.null(having) && nchar(having[[1]]) > 0) { statement = sprintf("%s\nHAVING %s", statement, having[[1]]) }
  if (!is.null(order.by) && nchar(order.by[[1]]) > 0) { statement = sprintf("%s\nORDER BY %s", statement, order.by[[1]]) }
  if (!is.null(limit) && nchar(limit[[1]]) > 0) { statement = sprintf("%s\nLIMIT %s", statement, limit[[1]]) }
  statement
}
