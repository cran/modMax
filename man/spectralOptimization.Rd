\name{spectralOptimization}
\alias{spectralOptimization}
\alias{multiWay}
\alias{spectral1}
\alias{spectral2}

\title{
Spectral optimization algorithms
}
\description{
\code{spectralOptimization} uses the leading eigenvector to recursively split the communities of a network into two until no further improvement of modularity is possible.

\code{multiWay}, \code{spectral1} and \code{spectral2} use \eqn{k-1} leading eigenvectors to split the network into \eqn{k} communities. The value for \eqn{k} leading to the best community structure is chosen as the final number of communities and the resulting split of the network into \eqn{k} communities as the final community structure. The 3 functions implement slightly different approaches leading to possibly different results.
}
\usage{
spectralOptimization(adjacency, numRandom = 0, initial = c("general", "own"),
                      refine = FALSE)
multiWay(adjacency, numRandom=0, maxComm=length(adjacency[1,]))
spectral1(adjacency, numRandom=0, maxComm=length(adjacency[1,]))
spectral2(adjacency, numRandom=0, maxComm=length(adjacency[1,]))
}

\arguments{
  \item{adjacency}{
  A nonnegative symmetric adjacency matrix of the network whose community structur will be analyzed
}
  \item{numRandom}{
  The number of random networks with which the modularity of the resulting community structure should be compared (default: no comparison). see details below for further explanation of the used null model.
}
  \item{initial}{
  Specify the community structure to use as initial partition in the algorithm. See details below.
}
  \item{refine}{
  If \code{TRUE}, Kernighan-Lin refinement is applied after splitting a community into two communities only on this part of the network.
}
  \item{maxComm}{
  THe maximum number of communities that the network allows
}
}
\details{
The used random networks have the same number of vertices and the same degree distribution as the original network.

The initial partition used in the spectral optimization algorithm can either be the generic one where all vertices are put in their own community (\code{initial=general}) or the initial partition can be given by the user (\code{initial=own}). In this case, the user needs to add a last column to the adjacency matrix indicating the initial partition. Hence, the adjacency matrix has to have one column more than the network has vertices.
}
\value{
The result of the spectral optimization algorithms is a list with the following components

  \item{number of communities}{
  The number of communities detected by the algorithm
  }
  \item{modularity}{
  The modularity of the detected community structure
  }
  \item{mean}{
  The mean of the modularity values for random networks, only computed if \code{numRandom>0} 
  }
  \item{standard deviation}{
  The standard deviation of the modularity values for random networks, only computed if \code{numRandom>0} 
  }
  \item{community structure}{
  The community structure of the examined network given by a vector assigning each vertex its community number
  }
  \item{random modularity values}{
  The list of the modularity values for random networks, only computed if \cr\code{numRandom>0}
  }
}
\references{
Newman, M. Finding community structure in networks using the eigenvectors
of matrices. \emph{Phys. Rev. E}, 74:036104, Sep 2006.

Newman, M. E. J. Modularity and community structure in networks.
\emph{Proceedings of the National Academy of Sciences}, 103(23):8577-8582, 2006.

Wang, G., Shen, Y., and Ouyang, M. A vector partitioning approach to detecting community structure in complex networks. \emph{Computers and Mathematics with Applications}, 55(12):2746-2752, 2008.

White, S. and Smyth, P. A spectral clustering approach to finding communities in graphs. In \emph{In SIAM International Conference on Data Mining}, 2005.
}
\author{
Maria Schelling, Cang Hui
}

\examples{
#unweighted network
randomgraph1 <- erdos.renyi.game(10, 0.3, type="gnp",directed = FALSE, loops = FALSE)

#to ensure that the graph is connected
vertices1 <- which(clusters(randomgraph1)$membership==1)  
graph1 <- induced.subgraph(randomgraph1,vertices1)

adj1 <- get.adjacency(graph1)
result1 <- spectralOptimization(adj1, refine = TRUE)

#weighted network
randomgraph2 <- erdos.renyi.game(10, 0.3, type="gnp",directed = FALSE, loops = FALSE)

#to ensure that the graph is connected
vertices2 <- which(clusters(randomgraph2)$membership==1)  
graph2 <- induced.subgraph(randomgraph2,vertices2)
graph2 <- set.edge.attribute(graph2, "weight", value=runif(ecount(graph2),0,1))

adj2 <- get.adjacency(graph2, attr="weight")
result2 <- multiWay(adj2, maxComm=3)
}

\keyword{Clustering}
\keyword{Partitioning}
\keyword{Modules}
\keyword{Metabolic network}
\keyword{Social network}
\keyword{Complex networks}
\keyword{Community structure}
\keyword{Eigenspectrum}
\keyword{Modularity matrix}
\keyword{Vector partition approach}