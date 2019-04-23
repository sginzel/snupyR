#' SnupyGraph Object
#'
#'\code{SnupyGraph} generates a S6 class
#'
#'@importFrom R6 R6Class
#'@param organism Can be human or mouse, or any valid StringDB taxon id.
#'@param threshold Threshold for StringDB links (default: 750)
#'@param version StringDB version (default: "10")
#'@param session a SnupySession object.
#'@details The class wraps a StringDB object and provides additional methods to perform diffusion and mapping between variants (\code{\link{SnupyData}}) and the PPI network.
#'@return Object of R6 class to use SnupyGrtaph
#'@field version Version of the StringDB
#' @section Methods:
#'  \describe{
#'   \item{\code{diffusion()}}{Returns a diffusion matrix for the graph.}
#'   \item{\code{plot.diffusion()}}{Returns a diffusion based plot.}
#'   \item{\code{rank.diffusion()}}{Returns a diffusion based rank.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{Experiment}}, \code{\link{EntityGroup}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}
#'@usage
#' grph = SnupyGraph$new("human", 750, "10")
#'@examples
#' grph = SnupyGraph$new("human", 750, "10")
#'@aliases graph
#'@family data
#'@format \code{\link{R6Class}} object.
#'@docType class
#'@export
SnupyGraph <- R6Class("Graph",
									public = list(
										organism = "human",
										threshold = 750,
										string_version = "10",
										string_db = NULL,
										session = NULL,
										initialize = function(organism = "human", threshold = 750, version = "10", session, stringdir = sprintf("%s/stringdb", session$CACHEDIR), ...) {
											self$organism = organism
											self$threshold = threshold
											self$string_version = version
											self$session = session
											if (!dir.exists(stringdir))
												dir.create(stringdir)
											self$string_db = STRINGdb$new( version="10", species=9606,  score_threshold=750, input_directory=stringdir )
											self
										},
										add_dataset = function(collection){
											# find variants in dataset
											# find gene names for dataset
											# map gene names to protein ids
											# add collection ids to appropriate nodes in the graph
										},
										gene_to_stringdb_id = function(genes){
											gene.aliases = subset(aliases, source == "Ensembl_HGNC")[,c("stringdb_id", "alias")] %>% set_colnames(c("stringdb_id", "gene"))
											merge(data.table(gene = genes), gene.aliases, by="gene", all.x =T)
										},
										stringbd_to_vertex = function(stringdb_ids){
											vids = get.vertex.attribute(self$graph, "name") %>% which(. %in% stringdb_ids)
											vids = match(stringdb_ids, get.vertex.attribute(self$graph, "name"))
											names(vids) = stringdb_ids
											if (any(is.na(vids)))
												warning("Not all stringdb_ids were mapped")
											vids
										},
										gene_to_vertex = function(genes, takeFirst = T){
											stringids = self$gene_to_stringdb_id(genes)
											stringvids = data.table(stringdb_id = stringds$stringdb_id, vid = self$string_db_to_vertex(stringds$stringdb_id))
											gene2vid = merge(stringids, stringvids, all.x = T) %>% as.data.table
											if (takeFirst){
												setkeyv(gene2vid, "stringdb_id")
												gene2vid = unique(gene2vid)
											}
											gene2vid$vid %>% set_names(gene2vid$gene)
										},
										aliases = function(source = NULL){
											if (private$.aliases %>% is.null){
												alias.file = STRINGdb:::downloadAbsentFile(paste0("http://string.uzh.ch/permanent/string/", self$string_db$version, "/protein_aliases/", self$string_db$species, "__protein_aliases_tf.tsv.gz"), self$string_db$input_directory)
												private$.aliases = read.csv(alias.file, sep="\t", header=T, stringsAsFactors = F) %>% as.data.table %>%
																					 set_names(c("taxon_id", "stringdb_id", "alias", "source")) %>%
																					 subset(., stringdb_id %in% vertex_attr(self$graph(), "name"))
											}
											if (!is.null(source)){
												subset(private$.aliases, source == source)
											} else {
												private$.aliases
											}
										}
									),
									active = list(
										graph = function(newval){
											if (!missing(newval)){
												private$.graph = newval
											} else if (is.null(private$.graph)) {
												# remove disconnected nodes
												private$.graph = self$string_db$get_graph() %>% delete_vertices(., degree(.) == 0)
											}
											private$.graph
										},
										aliases.sources = function(newval){
											aliases()$source %>% sort %>% unique
										}
									),
									private = list(
										.graph = NULL,
										.aliases = NULL
									)
)

if (1 == 0){
	ssion = SnupySession$new(file.path("..", "snupy-aqua-dev.conf.r"))
	sg = SnupyGraph$new(session = ssion)
	sgg = sg$graph()
}

if (1 == 0){
		require(igraph)
		require(expm)

	# Tutorial https://csustan.csustan.edu/~tom/Clustering/GraphLaplacian-tutorial.pdf
	#http://www.leonidzhukov.net/hse/2015/networks/lectures/lecture11.pdf

	set.seed(1234)
	my.gamma = .1
	g = random.graph.game(30, 0.15)
	q = rep(0, vcount(g)) %>% inset2(1, 1) %>% inset2(5, -1)
	q.pos = rep(0, vcount(g)) %>% inset2(1, 1)
	q.neg = rep(0, vcount(g)) %>% inset2(5, -1)

	diff.pvals = snupy_graph.diffusion.process.test(g, gamma = my.gamma,
																									q, normalize.result = T,
																									N = 1000,
																									alternative = "greater",
																									norm.method = "degree",
																									diffusion.boost.weight = data.table(vid = c(5), weight = c(1)),
																									diffusion.boost = T
																									)
	diff.pvals.pos = snupy_graph.diffusion.process.test(g, gamma = my.gamma,
																									q.pos, normalize.result = T,
																									N = 1000,
																									alternative = "greater"
	)
	diff.pvals.neg = snupy_graph.diffusion.process.test(g, gamma = my.gamma,
																									q.neg, normalize.result = T,
																									N = 1000,
																									alternative = "less"
	)
	# this shows that instead of integrating positive and negative scores we can also calculate them seperatly
	all((diff.pvals.pos$values + diff.pvals.neg$values) == diff.pvals$values)
	g.col = snupy_graph.diffusion.graph_branding(g, diff.pvals$values, col = c("steelblue", "lightyellow", "salmon"), c(-1, 1))
	plot(g.col,
			 vertex.color = get.vertex.attribute(g.col, "diffusion.color"),
			 vertex.shape = ifelse(q == 0, "circle", "rectangle"),
			 vertex.frame.color = ifelse(diff.pvals$pvals < 0.1, "red", "black"),
			 layout = layout.grid(g.col),
			 vertex.label = round(diff.pvals$values, 3), vertex.label.cex = 0.55,
			 main = sum(diff.pvals$values)
			 )


	L = laplacian_matrix(g, normalized = T, sparse = T)
	Gk = solve(as.matrix(L))
}


if (1 == 0){
	require(STRINGdb)
	string_db <- STRINGdb$new( version="10", species=9606,  score_threshold=750, input_directory="/opt/database/stringdb" )
	grph = string_db$get_graph()
	tmp = string_db$get_aliases(T)
	tmp = string_db$get_annotations()
	tmp = string_db$aliases_type
	tmp = string_db$aliases_tf
	string_db
	x = get.vertex.attribute(grph, "name") %>% data.frame(stringdb_id = .)
	# maps from gene to protein id
	example1_mapped <- string_db$map( x, "gene", removeUnmappedRows = TRUE )
	STRINGdb$help("get_aliases")
}

