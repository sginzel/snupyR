% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/snupy_graph_helper.R
\name{snupy_graph.diffusion.matrix}
\alias{snupy_graph.diffusion.matrix}
\title{snupy_graph.diffusion performs diffusion process on graph}
\usage{
snupy_graph.diffusion.matrix(grph, gamma = 0.2, norm.method = c("degree",
  "sinv", "none"), diffusion.boost.weight = data.table(vid = numeric(0),
  weight = numeric(0)), diffusion.boost = F, check.convergance = T)
}
\arguments{
\item{grph}{a iGraph object}

\item{gamma}{a diffusion width parameter}

\item{norm.method}{a normalization method used to normalize the adjecnecy matrix ("degree", "sinv", "none")}

\item{diffusion.boost.weight}{a 2-column table (vid, weight) where vid is the vertex id and weight is a positive factor to boost nodes.}

\item{diffusion.boost}{boost verticies?}
}
\value{
A diffusion matrix
}
\description{
snupy_graph.diffusion performs diffusion process on graph
}
\details{
Implementing the methods from Qi2008.
}
\seealso{
\code{\link{SnupyGraph}}

Other graph: \code{\link{snupy_graph.getA}}
}
\author{
Sebastian Ginzel <sginze2s@inf.h-brs.de>
}
