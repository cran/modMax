\name{modMax-package}
\alias{modMax-package}
\alias{modMax}
\docType{package}
\title{
Calculate network modularity via maximization algorithms
}
\description{
Calcuation of modularity and detection of the community strcture of a given network depicted by an (nonnegative symmetric) adjacency matrix using different modularity maximization algorithms 
}

\details{
\tabular{ll}{
Package: \tab modMax\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-02-09\cr
License: \tab GPL-2\cr
}

The \code{modMax} package implements 38 algorithms of 6 major categories maximizing modularity, including the greedy approach, simulated annealing, extremal optimization, genetic algorithm, mathematical programming and the usage of local modularity.

All algorithms work on connected (consisting of only one connected component), undirected graphs given by their adjacency matrix.

Most algorithms also provide the possibility to compare the estimated modularity of the identified community structure with the modularity for random networks generated by null models with the number of vertices and edges conserved.
}

\author{
Maria Schelling, Cang Hui

Maintainer: Maria Schelling <schelling.rmaintainer@vodafone.de>
}

\keyword{Modularity maximization}
\keyword{Compartmentalization}
\keyword{Network}
\keyword{Graph}
\keyword{Community detection}
\keyword{Clustering}

