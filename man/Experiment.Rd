% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiment.R
\docType{class}
\name{Experiment}
\alias{Experiment}
\alias{entity_group}
\title{Get and Experiment object}
\format{\code{\link{R6Class}} object.}
\usage{
Experiment$new(experiment_id=2)
Experiment$new(experiment=Experiment$new(2))
Experiment$new(id=123)
}
\arguments{
\item{filepath}{a path to a .r file that contains the config}

\item{connect}{logical value whether ssh tunnel should be connected or not.}
}
\value{
Object of R6 class to use Experiments
}
\description{
Get and Experiment object
}
\details{
Experiments contain full datasets, which means unlike the other elements of the
sample hierarchy the relations to entity groups, entities and specimen rely soley on the
association between the experiment and the entity group and the assoctied samples.
If any link is missing the samples cannot be queried. It is also possible to filter the
entity groups, entities, specimens and samples of an experiment to reduce the number of relvant samples.
\describe{
\item{name}{logical value if debug information should be shown}
\item{tags}{tags linked to this entity group}
}
}
\section{Fields}{

\describe{
\item{\code{name}}{Name of the \code{\link{Experiment}}
#' @section Methods:
 \describe{
  \item{\code{has_tag(Tag$new(id=1))}}{Returns true if the Experiment has is associated with Tag.}
}}
}}

\section{Methods}{

}

\examples{
eg <- Experiment$new(experiment_id=2)
}
\seealso{
\code{\link{Experiment}}, \code{\link{Entity}}, \code{\link{Specimen}}, \code{\link{Sample}} and \code{\link{VcfFile}}

Other data: \code{\link{EntityGroup}},
  \code{\link{Entity}}, \code{\link{GenePanel}},
  \code{\link{Sample}}, \code{\link{SnupyGraph}},
  \code{\link{SnupyTag}}, \code{\link{SpecimenProbe}},
  \code{\link{VariantDataSet}}, \code{\link{VcfFile}}
}
\author{
Sebastian Ginzel <sginze2s@inf.h-brs.de>
}
\keyword{datasets}
