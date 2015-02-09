\name{simulatedAnnealing}
\alias{simulatedAnnealing}
\alias{saIndividualCollectiveMoves}

\title{
Simulated annealing algorithms
}
\description{
The functions presented here are based on simulated annealing and identify the community structure and maximize the modularity.
\code{simulatedAnnealing} is only based on moving a single vertex from one community to another, while \code{saIndividualCollectiveMoves} considers movements of vertices, merging of communities and splitting of communities as alternatives to increase the modularity.
}
\usage{
simulatedAnnealing(adjacency, numRandom = 0, 
                    initial = c("general", "random","greedy", "own"), 
                    beta = length(adjacency[1, ])/2, alpha = 1.005, fixed)
saIndividualCollectiveMoves(adjacency,numRandom=0,initial=c("general","own"),
                            beta=length(adjacency[1,])/2,alpha=1.005,
                            fixed=25,numIter=1.0)
}

\arguments{
  \item{adjacency}{
A nonnegative symmetric adjacency matrix of the network whose community structur will be analyzed
}
  \item{numRandom}{
  The number of random networks with which the modularity of the resulting community structure should be compared (default: no comparison). see details below for further explanation of the used null model.
}
  \item{initial}{
  Specify the community structure to use as the initial partition in the algorithm. See details below.
}
  \item{beta}{
  Define the initial inverse temperature. \code{Default} is \code{(network size)/2}
}
  \item{alpha}{
  Define the cooling parameter. \code{Default} is \code{1.005}
}
  \item{fixed}{
  If the community structure has not changed for this specified number of steps, the algorithm is terminated.  
}
  \item{numIter}{
  Define the iteration factor. At each temperature, the algorithm performs \eqn{fn^2} individual moves (movement of a single vertex) and \eqn{fn} collective moves (merge or split of a community) where \eqn{n} is the number of vertices in the network.
  }
}
\details{
The used random networks have the same number of vertices and the same degree distribution as the original network.

The initial partition used in the simulated annealing algorithms can either be the generic one where all vertices are put in their own community (\code{initial=general}) or the initial partition can be identified by randomly identifying the initial number of communities and randomly assigning the vertices to one of these communities (\code{initial=random}) or the initial partition can be the community structure identified by the greedy algorithm (\code{initial=greedy}) or the initial partition can be given by the user (\code{initial=own}). In this case, the user needs to add a last column to the adjacency matrix indicating the initial partition. Hence, the adjacency matrix has to have one column more than the network has vertices.
}
\value{
The result of the simulated annealing algorithms is a list with the following components

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
Medus, A., Acua, G. and Dorso, C.O. Detection of community structures
in networks via global optimization. \emph{Physica A: Statistical Mechanics and
its Applications}, 358(24):593-604, 2005.

Massen, C. and Doye, J. Identifying communities within energy landscapes. \emph{Phys. Rev. E}, 71:046101, Apr 2005.

Guimera, R. and Amaral, L. A. N. Nunes amaral. Functional cartography
of complex metabolic networks. \emph{Nature}, 2005.
}
\author{
Maria Schelling, Cang Hui
}

\examples{

#unweighted network
randomgraph <- erdos.renyi.game(10, 0.3, type="gnp",directed = FALSE, loops = FALSE)

#to ensure that the graph is connected
vertices <- which(clusters(randomgraph)$membership==1)  
graph <- induced.subgraph(randomgraph,vertices)

adj <- get.adjacency(graph)
result <- simulatedAnnealing(adj, fixed=10)

}

\keyword{Networks}
\keyword{Communality}
\keyword{Betweenness}
