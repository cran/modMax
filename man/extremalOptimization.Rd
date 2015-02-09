\name{extremalOptimization}
\alias{extremalOptimization}
\alias{pcseoss}

\title{
Extremal optimization (EO) algorithms
}
\description{
\code{extremalOptimization} is a function executing the extremal optimization approach and its modifications for calculating modularity and detecting communities (modules of nodes) of a network via modularity maximization

\code{pcseoss} is a function which uses extremal optimization, but also considers pairwise constraints when calculating the fitness function and the modularity. The violation of constraints is punished, leading to smaller fitness and modularity values for community structures that violate many pairwise constraints. The constraints are predefined as two matrices separately for must-links and cannot-links with punishment for violation.
}

\usage{
extremalOptimization(adjacency, numRandom = 0, 
                      refine = c("none", "agents"), 
                      tau = FALSE, alpha_max = length(adjacency[1,]), steps = 3)
pcseoss(adjacency,constraints_ml,constraints_cl)
}

\arguments{
  \item{adjacency}{
A nonnegative symmetric adjacency matrix of the network whose community structur will be analyzed
}
  \item{numRandom}{
  The number of random networks with which the modularity of the resulting community structure should be compared (default: no comparison). see details below for further explanation of the used null model.
}
  \item{refine}{
  Specify whether or not a refinement step is needed, the default option is \code{none}. See details below.
}
  \item{tau}{
  If \code{TRUE}, \eqn{\tau}-EO is executed where the vertices are ranked according to their fitness values and chosen by a probability depending on this ranking.
}
  \item{alpha_max}{
  It gives the maximum number of iteration steps. If the community structure could not be improved for this number of steps, the algorithm terminates. It is \code{1} for the normal EO-algorithm and \code{n} for the \eqn{\tau}-EO where \code{n} is the number of vertices in the network
}
  \item{steps}{
  The number of iteration steps for the random local search agent algorithm. The algorithm terminates, if the clusters have not changed for this number of steps. Ignored if \code{refine} is \code{none}.
}
  \item{constraints_ml}{
  The matrix where each column is a must-link constraint given by two vertices in the first two rows which have to be in the same community and a punishment for the violation of the constraint in the third row
  }
  \item{constraints_cl}{
  The matrix where each column is a cannot-link constraint given by two vertices in the first two rows which cannot be in the same community and a punishment for the violation of the constraint in the third row
  }
}
\details{
The used random networks have the same number of vertices and the same degree distribution as the original network.

The EO algorithm can be run with a certain refinement step, the local random search agent algorithm, applied at the end of one round of extremal where all communities have been split once.

This refinement algorithm is executed if \code{refine} equals \code{agent}, otherwise the generic EO algorithm is executed.
}

\value{
The result of the extremal optimization algorithms is a list with the following components

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
Duch, J. and Arenas, A. Community detection in complex networks using extremal optimization. \emph{Phys. Rev. E}, 72:027104, Aug 2005.

Azizifard, N., Mahdavi, M. and Nasersharif, B. Modularity optimization for clustering in social networks. 2011.

Li, L., Du, M., Liu, G., Hu, X. and Wu, G. Extremal optimization-based semi-supervised algorithm with conflict pairwise constraints for community detection. In \emph{Advances in Social Network Analysis and Mining (ASONAM), 2014 IEEE/ACM International Conference
on}, 2014.
}
\author{
Maria Schelling, Cang Hui
}

\examples{

#weighted network
randomgraph <- erdos.renyi.game(10, 0.3, type="gnp",directed = FALSE, loops = FALSE)

#to ensure that the graph is connected
vertices <- which(clusters(randomgraph)$membership==1)  
graph <- induced.subgraph(randomgraph,vertices)
graph <- set.edge.attribute(graph, "weight", value=runif(ecount(graph),0,1))

adj <- get.adjacency(graph, attr="weight")
result <- extremalOptimization(adj)
}

\keyword{Extremal Optimization}
\keyword{Community}
\keyword{Modularity}
\keyword{Random Local Search Agent}
\keyword{Social Networks}
\keyword{PCSEO-SS algorithm}
\keyword{Community structure}
\keyword{Conflict pairwise constraints}
\keyword{large-scale network}
