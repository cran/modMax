\name{geneticAlgorithm}
\alias{geneticAlgorithm}

\title{
Genetic algorithm
}
\description{
\code{geneticAlgorithm} is a function executing the genetic algorithm and its modifications for identifying the community structure of a network via modularity maximization
}
\usage{
geneticAlgorithm(adjacency, numRandom = 0, 
                  initial = c("general", "cluster", "own"), p, g, 
                  mutRat = 0.5, crossOver = 0.2, beta = 0.1, alpha = 0.4, 
                  n_l = 4, local = FALSE)
}

\arguments{
  \item{adjacency}{
    A nonnegative symmetric adjacency matrix of the network whose community structur will be analyzed
}
  \item{numRandom}{
  The number of random networks with which the modularity of the resulting community structure should be compared (default: no comparison). see details below for further explanation of the used null model
}
  \item{initial}{
  Specify the community structure to use as initial partition in the algorithm. See details below.
}
  \item{p}{
  Population size
}
  \item{g}{
  Number of generations
}
  \item{mutRat}{
  Mutation rate. \code{Default} is \code{0.5}
}
  \item{crossOver}{
  Crossing over rate. \code{Default} is \code{0.2}
}
  \item{beta}{
  The fraction of chromosomes to save. The top \eqn{\beta}\eqn{p} chromosomes are saved in each generation to ensure that the fitness scores of the top \eqn{\beta}\eqn{p} chromosomes of the child generation are at least as good as the parent population. \code{Default} is \code{0.1}
}
  \item{alpha}{
  The fraction of repetitions for the identification of an initial partition according to \code{cluster}. \code{Default} is \code{0.4}. Ignored if \code{initial} is not \code{cluster}.
}
  \item{n_l}{
  The number of copies of a chromosome made by the local search operator. \code{Default} is \code{4}. Ignored if \code{local} is \code{FALSE}
}
  \item{local}{
If \code{TRUE}, local search operator is applied at the end of each iteration in the genetic algorithm.
}
}
\details{
The used random networks have the same number of vertices and the same degree distribution as the original network.

The initial partition used in the genetic algorithm can either be the generic one where all vertices are put in their own community (\code{initial=general}) or the initial partition can be identified by randomly picking a vertex \eqn{\alpha}\eqn{n} times and assigning its cluster to all its neighbours (\code{initial=cluster}) or the initial partition can be given by the user (\code{initial=own}). In this case, the user needs to add a last column to the adjacency matrix indicating the initial partition. Hence, the adjacency matrix has to have one column more than the network has vertices.
}
\value{
The result of the genetic algorithm is a list with the following components

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
Tasgin, M., Herdagdelen, A., and Bingol, H. Community detection
in complex networks using genetic algorithms. \emph{arXiv preprint
arXiv:0711.0491}, 2007.

Li, S., Chen, Y., Du, H., and Feldman, M. W. A genetic algorithm with local search strategy for improved detection of community
structure. \emph{Complexity}, 15(4):53-60, 2010.
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
result1 <- geneticAlgorithm(adj1, p=4, g=8)

}

\keyword{Network structure}
\keyword{Modularity}
\keyword{Genetic algorithm}
\keyword{Small-world phenomena}
