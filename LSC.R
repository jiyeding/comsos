LSC <- function(data, k, p = NULL, r = NULL, method = "random", 
                distance = "cosine", similarity = "cosine", seed = 0, 
                iter.max = 10, nstart = 10,labels,alpha1,alpha2) {
  #Set seed
  set.seed(seed)
  
  if (distance == "cosine") {
    d.metric = 2
  } else {
    d.metric = 1
  }
  
  #Some arbitrary functions to pick a p and r
  if (is.null(p)) {
    p <- floor(sqrt(nrow(data)*k)/2)
  }
  
  if (is.null(r)) {
    r <- floor(p/10)
  }
  
  if (method == "random") {
    #Sampling p landmarks randomly
    pmatrix <- data[sample(nrow(data), p),]
  } else {
    #Sampling p landmarks with kmeans
    pmatrix <- akmeans(data, min.k = p, max.k = p, iter.max = 10, 
                       d.metric = d.metric, verbose = FALSE)$centers
  }
  
  #Cosine by default. Euclidian otherwise
  if (similarity == "cosine") {
    #Compute affinity matrix with cosine similarity
    A <- (data/sqrt(rowSums(data^2))) %*% t(pmatrix/sqrt(rowSums(pmatrix^2)))
  } else {
    #Take euclidean distance between points and landmarks
    A <- as.matrix(pdist(data, pmatrix))
    A <- 1/exp(A)
  }
  
  #outlier removal #
  cs <- colSums(A)
  q2 <- quantile(cs,alpha2)
  print(q2)
  #plot(sort(cs))
  #abline(v=(length(cs)*alpha2),h=0,col="red")
  
  A <- A[,cs>=q2]
  
  rs <- rowSums(A)
  
  q1 <- quantile(rs,alpha1)
  print(q1)
  #print(A)
  #plot(sort(rs))
  #abline(v=(length(rs)*alpha1),h=0,col="red")
  #print(nrow(A))
  #print(length(rs))
  #n <- alpha1*n+1
  oA <- A[rs<q1,]
  A <- A[rs>=q1,]
  #print(oA)
  #print(A)
  nlabels <- labels[rs>=q1]
  olabels <- labels[rs<q1]
  
  ndata <- data[rs>=q1,]
  odata <- data[rs<q1,]
  #print(odata)
  
  #Keep r largest entries in each row
  keepr <- function(x, length, r) {
    x[order(x)[1:(length-r)]] <- 0L
    return(x)
  }
  
  length <- ncol(A)
  A <- t(apply(A, 1, keepr, length = length, r = r))
  

  
  #Calculate A1
  A1  <- A/rowSums(A)
  
  #Calculate A2
  A2 <- t(t(A1)/sqrt(rowSums(t(A1))))
  
  #Take top k left singular vectors of A2
  U <- svd(A2, nu = k, nv = 0)$u
  
  #Kmeans on U
  fit <- kmeans(U, centers = k, iter.max = iter.max, nstart = nstart)
  fit[["nlabels"]] = nlabels
  fit[["odata"]] = odata
  fit[["ndata"]] = ndata
  fit[["olabels"]] = olabels
  fit[["oA"]] = oA
  return(fit)
}
