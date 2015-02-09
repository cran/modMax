\name{localModularity}
\alias{localModularity}
\alias{localModularityWang}

\title{
Algorithms using local modularity
}
\description{
\code{localModularity} uses the local modularity to identify the local community structure around a certain vertex

\code{localModularityWang} uses the local modularity to identify the community structure of the entire network
}
\usage{
localModularity(adjacency, srcV, k)
localModularityWang(adjacency,numRandom=0)
}

\arguments{
  \item{adjacency}{
A nonnegative symmetric adjacency matrix of the network whose community structur will be analyzed
}
  \item{srcV}{
  A given vertex whose local community structure should be determined by\cr \code{localModularity}
}
  \item{k}{
  The maximum number of vertices to add to the local community of \code{srcV}
}
  \item{numRandom}{
  The number of random networks with which the modularity of the resulting community structure should be compared (default: no comparison). see details below for further explanation of the used null model.
}
}
\details{
The used random networks have the same number of vertices and the same degree distribution as the original network.
}

\value{
The result for \code{localModularity} is returned as a list with the following components

  \item{local community\cr structure}{
  Vertices assigned to the same community as the source vertex \code{srcV}
  }
  \item{local modularity}{
  The local modularity value for the determined local community
  }

The result for \code{localModularityWang} is returned as a list with the following components

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
Clauset, A. Finding local community structure in networks. \emph{Phys. Rev.
E}, 72:026132, Aug 2005.

Wang, X., Chen, G. and Lu, H. A very fast algorithm for detecting community structures in complex networks. \emph{Physica A: Statistical Mechanics and its Applications}, 384(2):667-674, 2007.
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
result1 <- localModularity(adj1, srcV=1, k=4)

#weighted network
randomgraph2 <- erdos.renyi.game(10, 0.3, type="gnp",directed = FALSE, loops = FALSE)

#to ensure that the graph is connected
vertices2 <- which(clusters(randomgraph2)$membership==1)  
graph2 <- induced.subgraph(randomgraph2,vertices2)
graph2 <- set.edge.attribute(graph2, "weight", value=runif(ecount(graph2),0,1))

adj2 <- get.adjacency(graph2, attr="weight")
result2 <- localModularityWang(adj2)
}

\keyword{Complex network}
\keyword{Community structure}
\keyword{Local information}
\keyword{Relative table}
\keyword{Cache}
