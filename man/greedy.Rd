\name{greedy}
\alias{greedy}
\alias{rgplus}
\alias{msgvm}
\alias{cd}
\alias{louvain}
\alias{vertexSim}
\alias{mome}

\title{
Greedy algorithms
}
\description{
\code{greedy} executes the general CNM algorithm and its modifications for modularity maximization.

\code{rgplus} uses the randomized greedy approach to identify core groups (vertices which are always placed into the same community) and uses these core groups as initial partition for the randomized greedy approach to identify the community structure and maximize the modularity.

\code{msgvm} is a greedy algorithm which performs more than one merge at one step and applies fast greedy refinement at the end of the algorithm to improve the modularity value.

\code{cd} iteratively performs complete greedy refinement on a certain partition and then, moves vertices with a probability \eqn{p} to another community to avoid the greedy algorithm getting trapped in a local optimum.

\code{louvain} performs fast greedy refinement and uses the resulting community structure to build a new network where vertices in the new network are the communities in the original network. For this new network, all vertices are assigned to their own community, and the fast greedy refinement is applied again.

\code{vertexSim} uses a vertex similarity measure to identify the initial partition and further improves this community structure by merging neighbouring communities.

\code{mome} consists of the two phases of coarsening and uncoarsening with refinement. In the coarsening phase, two vertices are collapsed into one vertex for which the increase in modularity is maximal. In the uncoarsening phase, each intermediate graph of the coarsening phase is revisited and its community structure is refined by applying fast greedy refinement. After revisiting the different steps, the community structure for the original graph can be reconstructed from different coarsening levels.
}

\usage{
greedy(adjacency, numRandom = 0, 
        q = c("general", "danon", "wakita1", "wakita2", "wakita3"), 
        initial = c("general", "prior", "walkers", "subgraph", "adclust", "own"),
        randomized = 0, refine = c("none", "complete", "fast", "kernighan"), 
        coarse = 0)
rgplus(adjacency,numRandom=0,z,randomized)
msgvm(adjacency,numRandom=0,initial=c("general","own"), parL)
cd(adjacency, numRandom=0,initial=c("general","own"),maxC=length(adjacency[,1]),
  iter,p)
louvain(adjacency, numRandom=0, initial=c("general","own"))
vertexSim(adjacency, numRandom=0, frac=0.5)
mome(adjacency, numRandom=0)
}

\arguments{
  \item{adjacency}{
A nonnegative symmetric adjacency matrix of the network whose community structur will be analyzed
}
  \item{numRandom}{
  The number of random networks with which the modularity of the resulting community structure should be compared (default: no comparison). see details below for further explanation of the used null model.
}
  \item{q}{
  Specify whether the general \eqn{\Delta}\eqn{Q} value or a modification should be used. See details below.
}
  \item{initial}{
  Specify the community structure to be used as initial partition in the algorithm. See details below.
}
  \item{z}{
  The number of executions of the randomized greedy approach to identify the core groups.
  }
  \item{randomized}{
  The number of rows to use for the randomized greedy approach. Ignored when set to \code{0} (default)
}
  \item{refine}{
  specifies which refinement algorithm should be used. See details below.
}
  \item{coarse}{
  Define the percentage by which the number of communities has to be decreased since the last coarsening level to consider the current clustering as a new coarsening level and apply refinement on this clustering
}
  \item{parL}{
  The number of merges at one step in the \code{msgvm} algorithm
  }
  \item{maxC}{
  The maximum number of communities for the initial partition used in the \code{cd} algorithm    
  }
  \item{iter}{
  The number of iterations in the \code{cd} algorithm
  }
  \item{p}{
  The probability with which a vertex is moved into another community in the dilation step of the \code{cd} algorithm
  }
  \item{frac}{
  The fraction of iteration steps for which "pairwise" merging is performed in the \code{vertexSim} algorithm. Remaining iteration steps are "single neighbour" merges.
  }
}
\details{
The used random networks have the same number of vertices and the same degree distribution as the original network.

For the identification of the best merging event leading to a maximum increase in modularity, different values of the modularity were proposed. Which modularity value to use is specified by the parameter \code{q}. The options are \code{general} where the normal value for \eqn{\Delta}\eqn{Q} is used, \code{danon} where \eqn{\Delta}\eqn{Q} is normalized by the number of overall edges of vertices in a community and \code{wakita1}, \code{wakita2} and \code{wakita3} where \eqn{\Delta}\eqn{Q} is multiplied by the consolidation ratio.

The greedy algorithms can be run on different initial partitions. The used initial partition is specified by parameter \code{initial}. The options are \code{general} where all vertices are assigned to their own community, \code{prior} where the initial community structure is identified by using prior knowledge, \code{walkers} where the initial community structure is identified by using random walkers, \code{subgraph} where the initial community structure is identified by using subgraph similarity, \code{adclust} where the general initial partition is refined using fast greedy refinement and \code{own} where the user can specify an initial partition to use with the greedy approach. In this case, the user needs to add a last column to the adjacency matrix indicating the initial partition. Hence, the adjacency matrix has to have one column more than the network has vertices.

The community structure identified by the CNM algorithm can be refined by applying a refinement step at the end of the algorithm. The used refinement algorithm is specified by the parameter \code{refine}. The options are \code{none} where no refinement algorithm is applied, \code{complete} where the complete greedy refinement is applied, \code{fast} where the fast greedy refinement is applied, \code{kernighan} where the adapted Kernighan-Lin refinement is applied. Besides, if \code{initial} is set to \code{adclust}, fast greedy refinement is applied to the community structure after each merging event.
If \code{coarse != 0}, the refinement algorithm specified by \code{refine} is not only applied at the end of the algorithm, but at each coarsening level where coarsening levels are defined according to \code{coarse}.
}

\value{
The result of the greedy algorithms is a list with the following components

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
Clauset, A., Newman, M. and Moore, C. Finding community
structure in very large networks. \emph{Phys. Rev. E}, 70:066111, Dec 2004.

Danon, L., Daz-Guilera, A. and Arenas, A. The effect of size
heterogeneity on community identifcation in complex networks. \emph{Journal
of Statistical Mechanics: Theory and Experiment}, 2006(11):P11010, 2006.

Wakita, K. and Tsurumi, T. Finding community structure in
mega-scale social networks: [extended abstract]. In \emph{Proceedings of the 16th
International Conference on World Wide Web}, WWW '07, pages 1275-
1276, New York, NY, USA, 2007. ACM.

Ovelgonne, M. and Geyer-Schulz, A. Cluster cores and modularity maximization.
In \emph{Data Mining Workshops (ICDMW), 2010 IEEE International
Conference on}, pages 1204-1213, Dec 2010.

Du, H., Feldman, M. W., Li, S. and Jin, X. An algorithm
for detecting community structure of social networks based on prior
knowledge and modularity. \emph{Complexity}, 12(3):53-60, 2007.

Pujol, J., Bejar, J. and Delgado, J. Clustering algorithm for determining
community structure in large networks. \emph{Phys. Rev. E}, 74:016107,
Jul 2006.

Xiang, B., Chen, E.-H. and Zhou, T. Finding community structure
based on subgraph similarity. In Santo Fortunato, Giuseppe Mangioni,
Ronaldo Menezes, and Vincenzo Nicosia, editors, \emph{Complex Networks}, volume
207 of \emph{Studies in Computational Intelligence}, pages 73-81. Springer
Berlin Heidelberg, 2009.

Noack, A. and Rotta, R. Multi-level algorithms for modularity
clustering. Technical report, 2008.

Ye, Z., Hu, S. and Yu, J. Adaptive clustering algorithm for
community detection in complex networks. \emph{Phys. Rev. E}, 78:046115, Oct
2008.

Schuetz, P. and Caflisch, A. Efficient modularity optimization by multistep greedy algorithm and vertex mover refinement. \emph{Phys. Rev. E}, 77:046112, Apr 2008.

Mei, J., He, S., Shi, G., Wang, Z., and Li, W. Revealing network communities through modularity maximization by a contractiondilation method. \emph{New Journal of Physics}, 11(4):043025, 2009.

Blondel, V. D., Guillaume. J.-L., Lambiotte, R. and Lefebvre, E. Fast unfolding of communities in large networks. \emph{Journal of Statistical Mechanics: Theory and Experiment}, 2008(10):P10008, 2008.

Arab, M. and Afsharchi, M. A modularity maximization algorithm
for community detection in social networks with low time complexity.
In \emph{Web Intelligence and Intelligent Agent Technology (WI-IAT), 2012
IEEE/WIC/ACM International Conferences on}, volume 1, pages 480-487,
Dec 2012.

Zhu, Z., Wang, C., Ma, L., Pan, Y. and Ding, Z. Scalable community
discovery of large networks. In \emph{Web-Age Information Management,
2008. WAIM '08. The Ninth International Conference on}, pages 381-388,
July 2008.
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
result1 <- greedy(adj1, refine = "fast")

#weighted network
randomgraph2 <- erdos.renyi.game(10, 0.3, type="gnp",directed = FALSE, loops = FALSE)

#to ensure that the graph is connected
vertices2 <- which(clusters(randomgraph2)$membership==1)  
graph2 <- induced.subgraph(randomgraph2,vertices2)
graph2 <- set.edge.attribute(graph2, "weight", value=runif(ecount(graph2),0,1))

adj2 <- get.adjacency(graph2, attr="weight")
result2 <- louvain(adj2)
}
\keyword{Analysis of algorithms}
\keyword{Network dynamics}
\keyword{Social network}
\keyword{Network theory}
\keyword{Community discovery}
\keyword{Graph clustering}
\keyword{Modularity}
\keyword{Multilevel}
\keyword{Community structure}
\keyword{rural-urban migration}
\keyword{Community analysis}
\keyword{Clustering}
\keyword{Social networking service}
\keyword{Community detection}
\keyword{Randomized algorithm}
\keyword{Random graphs}
\keyword{Networks}
\keyword{Critical phenomena of socio-economic systems}
\keyword{Socio-economic networks}
