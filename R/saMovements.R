#performance of individual node movement
individualNodeMovement <- function(network,C,Q,beta){
  
  vertex <- sample(1:vcount(network),1)
  community <- sample(1:(max(C)+1),1)
  accept <- 0  
  
  if(community != C[vertex]){
    Q_move <- calculateQMove(network,C,community,vertex)

    if(Q_move != -Inf){
      if(Q_move > 0){
        accept <- 1
      }
      else{
        prob <- exp(beta*Q_move)
        accept <- sample(c(0,1),1,replace=TRUE,c((1-prob),prob))
      }
    }
  }
  
  if(accept==1){
    C[vertex]= community
    
    Q <- Q + Q_move
    C <- updateC(C)
  }  
  result <- c(C,Q,accept) 
}

####performance of collective movements
#merging of communities
mergeCommunities <- function(network,C,Q,beta){
  
  accept <- 0
  if(max(C)>1){
    ids <- sample(1:max(C),2,replace=FALSE)
    Q_merge <- calculateQMerge(network,C,ids[1],ids[2])
  }
  else{
    Q_merge <- 0
  }
  
  if(Q_merge > 0){
    accept <- 1
  }
  else if(Q_merge==0){
    accept <- 0
  }
  else{
    prob <- exp(beta*Q_merge)
    accept <- sample(c(0,1),1,replace=TRUE,c((1-prob),prob))
  }
  
  if(accept==1){
    for(i in 1:length(C)){
      if(C[i]==ids[2]){
        C[i]==ids[1]
      }
    }
    Q <- Q+Q_merge
    C <- updateC(C)
  }
  
  result <- c(C,Q,accept)
  return(result)
}

#splitting of communities
splitCommunities <- function(network,C,Q,beta,alpha,beta_sys){
  id <- sample(1:max(C),1)
  comm <- NULL
  for(i in 1:length(C)){
    if(C[i]==id){
      comm <- cbind(comm, i)
    }
  }
  sub <- induced.subgraph(network,comm)
  C_sub <- sample(c(id,(max(C)+1)),vcount(sub), replace=TRUE)
  Q_sub <- calculateQ(sub,C_sub)
  
  while(beta<beta_sys){
    result <- individualNodeMovement(sub,C_sub,Q_sub,beta)
    Q_sub <- result[vcount(sub)+1]
    C_sub <- result[1:vcount(sub)]
    beta=beta*alpha
  }
  
  Q_split <- Q_sub - Q

  accept <-0
  if(Q_split>0){
    accept <- 1
  } else{
    prob <- exp(beta*Q_split)
    accept <- sample(c(0,1),1,replace=TRUE,c((1-prob),prob))
  }
  
  if(accept==1){
    Q <- Q_sub
    for(i in 1:length(C_sub)){
      vertex=comm[i]
      C[vertex]=C_sub[i]
    }
  }
  C <- updateC(C)
  result <- c(C,Q,accept)
  return(result)
}