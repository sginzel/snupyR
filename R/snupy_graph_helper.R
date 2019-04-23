#' snupy_graph.diffusion performs diffusion process on graph
#'
#'@param grph a iGraph object
#'@param gamma a diffusion width parameter
#'@param norm.method a normalization method used to normalize the adjecnecy matrix ("degree", "sinv", "none")
#'@param diffusion.boost boost verticies?
#'@param diffusion.boost.weight a 2-column table (vid, weight) where vid is the vertex id and weight is a positive factor to boost nodes.
#'@details Implementing the methods from Qi2008.
#'@return A diffusion matrix
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{SnupyGraph}}
#'@family graph
#'@export
snupy_graph.diffusion.matrix <- function(grph,
																				 gamma = 0.2,
																				 norm.method = c("degree", "sinv", "none"),
																				 diffusion.boost.weight = data.table(vid = numeric(0), weight = numeric(0)),
																				 diffusion.boost = F,
																				 check.convergance = T
																				 ){
	# get adjency matrix
	A = snupy_graph.getA(grph = grph, A.attr = NULL)

	if (norm.method[[1]] %in% c("degree", "symetric")){
		S.sqrt = snupy_graph.getS(A)
		diag(S.sqrt) = diag(S.sqrt)^(-0.5)
		A.norm = (S.sqrt) %*% A %*% (S.sqrt)
		rm(S.sqrt)
	} else if (norm.method[[1]] %in% c("sinv", "asymetric")){
		S = snupy_graph.getS(A)
		diag(S) = 1 / diag(S)
		A.norm = A %*% S
		rm(S)
	} else if (norm.method[[1]] %in% c("none")){
		A.norm = A
	} else {
		stop("Normalization method should be degree or S_inverse or none")
	}

	S = snupy_graph.getS(A.norm)

	# check for convergance
	if (check.convergance)
		stopifnot(.graph.diffusion.converges(A.norm, gamma))

	if (any(diffusion.boost.weight$diffusion.weight < 0))
		stop("Only positive diffusion weights are allowed")

	# multiply every column with its diffusion weight. This was nodes release more fluid over their edges, which get "wider" to carry more fluid
	# But it also losed more fluid in the process, leading to a possible accumulation in other nodes.
	if (diffusion.boost){
		shift.matrix = getIdentity(d = ncol(A), diag.value = gamma)
		shifted.laplacian = -(A.norm - S - shift.matrix)
		if (nrow(diffusion.boost.weight) > 0){
			for(i in 1:nrow(diffusion.boost.weight)){
				vid = diffusion.boost.weight[i,]$vid
				diff.factor = diffusion.boost.weight[i,]$weight
				shifted.laplacian[-(vid),vid] = shifted.laplacian[-(vid),vid] * diff.factor
			}
		}
	} else {
		shift.matrix = getIdentity(d = ncol(A), diag.value = rep(gamma, ncol(A)))
		shifted.laplacian = -(A.norm - S - shift.matrix)
	}


	G = solve(as.matrix(shifted.laplacian))


}

snupy_graph.diffusion.process <- function(g, gamma, q, normalize.result = F, ...){
	G = snupy_graph.diffusion.matrix(g, gamma, ...)
	if (!normalize.result){
		diffusion.vector = G %*% q
	} else {
		diffusion.vector = G %*% diag(x = 1/colSums(G), ncol = ncol(G), nrow=nrow(G)) %*% q
	}
	diffusion.vector
}

snupy_graph.diffusion.process.test <- function(g, gamma, q, N = 1000, alternative = c("greater", "less"), ...){
	stopifnot(any(alternative %in% c("less", "greater")))
	# perform N random diffusions using the permutations of q
	perm.q = sapply(1:(N+1), function(x){
		sample(q)
	})
	perm.q[,1] = q
	diffusion.perm = snupy_graph.diffusion.process(g = g, gamma = gamma, q = perm.q, ...)
	if (alternative[[1]] == "less"){
		comp.fun = `>`
	} else {
		comp.fun = `<`
	}
	diffusion.pvals = apply(diffusion.perm, MARGIN = 1, function(x){
		sum(comp.fun(x[1], x[2:length(x)])) / (length(x) - 1)
	})
	list(
		values = diffusion.perm[,1],
		pvals = diffusion.pvals,
		N = N
	)

}

snupy_graph.diffusion.graph_branding <- function(g, diffusion.vector, col = c("steelblue", "lightyellow", "salmon"), col.limit = NULL){
	# when 3 scale it from [-INF, 0, +INF]
	my.rescale <- function(x, newMin, newMax){ (x - min(x))/(max(x)-min(x)) * (newMax - newMin) + newMin }
	if (length(col) == 3){
		if (is.null(col.limit))
			col.limit = c(min(diffusion.vector), max(diffusion.vector))
		diffusion.vector.scaled = diffusion.vector
		diffusion.vector.scaled[diffusion.vector.scaled < 0] = my.rescale(diffusion.vector.scaled[diffusion.vector.scaled < 0], col.limit[[1]], 0)
		diffusion.vector.scaled[diffusion.vector.scaled > 0] = my.rescale(diffusion.vector.scaled[diffusion.vector.scaled > 0], 0, col.limit[[2]])
	} else if (length(col) == 2){
		if (is.null(col.limit))
			col.limit = c(0, max(diffusion.vector))
		diffusion.vector.scaled = my.rescale(diffusion.vector, col.limit[[1]], col.limit[[2]])
	} else {
		stop("please provide 2 or 3 colors for branding.")
	}
	stopifnot(length(diffusion.vector) == vcount(g))
	g = set_vertex_attr(g, "diffusion.value", index = V(g), diffusion.vector)
	map2color <- function(x, pal, limits = NULL){
		if(is.null(limits)) limits=range(x)
		pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1), all.inside=TRUE)]
	}
	vec.colors = map2color(diffusion.vector.scaled, colorRampPalette(colors = col)(length(diffusion.vector)), limits = col.limit)
	# vec.colors = colorRampPalette(colors = col)(length(diffusion.vector))[rank(diffusion.vector)]
	g = set_vertex_attr(g, "diffusion.color", index = V(g), vec.colors)
	g
}

#' Helper methods for snupy_graph diffusions
#'
#'@param filepath a path to a .r file that contains the config
#'@param connect logical value whether ssh tunnel should be connected or not.
#'@details The file containig the configuration should set the following variables\cr
#'@return Object of R6 class to use Entitys
#'@field name Name of the \code{\link{Entity}}
#' #' @section Methods:
#'  \describe{
#'   \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the Entity has is associated with Tag.}
#' }
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'@seealso \code{\link{SnupyGraph}}
#'@family graph
#'@export
snupy_graph.getA <- function(grph, A.attr = "weight"){
	if (any(list.edge.attributes(grph) == A.attr)){
		A = get.adjacency(grph, attr = A.attr)
	} else {
		A = get.adjacency(grph)
	}
	A
}
# return S matrix with diagonal set to column sums of A
snupy_graph.getS <- function(A){
	S = sparseMatrix( i = 1:dim(A)[1],
										j = 1:dim(A)[2],
										x = colSums(A),
										dims = dim(A))
	S
}
# http://r-pkgs.had.co.nz/man.html
#' Generate identity matrix
#'
#'\code{getIdentity} createsn identity matrix with the given dimension as a sparseMatrix.
#'
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'
#'@usage getIdentify(d, diag.value = 1)
#'@param d Dimension of identity matrix.
#'@param diag.value Value on the diagonal of the matrix. Default: 1
#'@details This method creates a square matrix for the dimension given by d.\cr
#'@return Returns a sparseMatrix.
#'@seealso \code{\link{graph_diffusion}}
#'@aliases identity i
#'@family matrix operations
#'@export
getIdentity <- function(d, diag.value = 1){
	sparseMatrix( i = 1:d,
								j = 1:d,
								x = diag.value,
								dims = c(d,d))
}

snupy_graph.shifted_lapacian <- function(A = NULL, grph = NULL, gamma){
	if (is.null(A))
		A = getA(grph)
	-(A - snupy_graph.getS(A) - getIdentity(d = ncol(A), diag.value = gamma))
}

snupy_graph.shifted_lapacian.norm <- function(A = NULL, grph = NULL, gamma, symetric = T){
	if (is.null(A))
		A = getA(grph)
	A = getnormA(A, symetric)
	-(A - snupy_graph.getS.norm(A, symetric) - getIdentity(d = ncol(A), diag.value = gamma))
}

snupy_graph.getA.norm <- function(A, symetric = T){
	if (symetric){
		snupy_graph.getA.norm.degree(A)
	} else {
		snupy_graph.getA.norm.sinv(A)
	}
}
snupy_graph.getS.norm <- function(A, symetric = T){
	normA = snupy_graph.getA.norm(A, symetric)
	normS = snupy_graph.getS(normA)
	normS
}

## Each Edge weight is normalized by the degree of of both nodes
# Symetric normalization
# A' = S^-1/2 * A * S^-1/2
snupy_graph.getA.norm.degree <- function(A){
	S.sqrt = snupy_graph.getS(A)
	diag(S.sqrt) = diag(S.sqrt)^(-0.5)
	(S.sqrt) %*% A %*% (S.sqrt)
}
# transition rate out of vertex is the column sum of A'
# Asymetric normalization
# A' = A * S^-1
snupy_graph.getA.norm.sinv <- function(A){
	S = snupy_graph.getS(A)
	diag(S) = 1 / diag(S)
	A %*% S
}

#' Power method to approximate the maximum eigenvalue of a matrix
#'
#'\code{maxEigen} performs maximum eigenvalue determination.
#'
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'
#'@usage maxEigen(m, e = 10^-16, max.iter = 1000)
#'@param m a numeric matrix.
#'@param e error threhold for the angle between the vectors before and after the matrix multiplication. Default: 10^-16
#'@param max_iter maximum number of iterations to perform. Default: 1000
#'@details The method starts with a vector of 1's and multiplies it continously with the given matrix. When the angle between the input and the result vector converges we determine the eigenvalue by the length ratio between the two vectors. \cr
#'@return max.eigen return the approximated maximum eigenvalue of the matrix or NA in case convergence was not achieved after max.iter iterations.
#'@seealso \code{\link{graph_diffusion}}
#'@aliases maximum eigenvalue maxeigen max.eigen
#'@family matrix operations
#'@export
maxEigen <- function(m, e = 10^-6, max.iter = 1000){
	# for details: http://ergodic.ugr.es/cphys/LECCIONES/FORTRAN/power_method.pdf
	# m = matrix(c(2, -12, 1, -5), byrow = T, ncol=2)
	# A.norm = getnormA(getA(grph), symetric = F)
	# m = A.norm-getS(A.norm)
	maxeigenvalue.converged = F
	x0 = rep(1, times = ncol(m))

	raleigh.quot <- function(m, x){
		top = t(m%*%x) %*% x
		bot = t(x) %*% x
		(top/bot)[1,1]
	}

	eigenval.before = raleigh.quot(m, x0)
	xi = x0
	numiter = 0
	val.diff = -1
	cat(sprintf("[MaxEigen] %f ???/%f\r", numiter, e))
	while (!maxeigenvalue.converged){
		numiter = numiter + 1
		xi = m %*% xi
		xi.norm = abs(xi/min(abs(xi)))
		xi.norm.eigenval = raleigh.quot(m, xi)
		if (is.nan(xi.norm.eigenval)){
			warning("invalid eigen value determined by Raleigh quotient. This is a numerical problem.")
			numiter = max.iter + 1
			break
		}
		val.diff = abs(xi.norm.eigenval - eigenval.before)
		maxeigenvalue.converged = (numiter > max.iter) | (val.diff < e)
		eigenval.before = xi.norm.eigenval
		cat(sprintf("[MaxEigen] %f -> %.16f/%.16f\r", numiter, val.diff, e))
	}
	cat(sprintf("[MaxEigen] %f -> %.16f/%.16f DONE\n", numiter, val.diff, e))
	if(!(numiter > max.iter)){
		maxeigenvalue = xi.norm.eigenval
	} else {
		warning("Eigenvalue determination did not converege after max.iter iterations. Return NA")
		maxeigenvalue = NA
	}
	return(maxeigenvalue)
}

#' Check if a given matrix representation of a graph will converge in a diffusion process.
#'
#'\code{.graph.diffusion.converges} checks if the maximum eigenvalue is smaller than the gamma parameter provided, which gives the diffusion width.
#'
#'@author Sebastian Ginzel <sginze2s@@inf.h-brs.de>
#'
#'@usage .graph.diffusion.converges(A.norm, gamma)
#'@param A.norm a numeric matrix.
#'@param gamma a gamma value
#'@return Return TRUE or FALSE
#'@seealso \code{\link{snupy_graph}}
#'@aliases convergance
#'@family matrix operations
.graph.diffusion.converges <- function(A.norm, gamma){
	## gamma has to be bigger than the largest eigenvalue
	will.converge = F
	if (dim(A.norm)[1] > 100){
		message("Graph is big so we approximate the maximum eigenvalue.")
		maxeigenvalue = maxEigen(A.norm-snupy_graph.getS(A.norm))
		if (is.na(maxeigenvalue)){
			warning("maximal eigenvalue could not be determined. Will assume approximation.")
			maxeigenvalue = gamma - 1
		}
	} else {
		message("checking if diffusion will converge...this may yield eigen values with imaginary parts")
		maxeigenvalue = max(as.double(eigen(A.norm-snupy_graph.getS(A.norm), only.values = T)$values))
	}
	will.converge = maxeigenvalue < gamma
	if (!will.converge)
		warning("Diffusion will not converge as the maximum eigen value of A-S is larger than gamma (see Qi et al. 2008).")

	will.converge
}

