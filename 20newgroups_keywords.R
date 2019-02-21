##### cluster interpretation example 20 newsgroup ######
news <- readMat("news20.mat") 
full <- news$A    # tf-idf data
data <- news$A.p  
labels <- news$y
rm(news)
words <- read.table("news20_words.txt")   # vector of words #
mapping <- read.table("test.map")[,1]

clusters <- LSC(data, k = 20)
accuracy(clusters,labels)

total_colsums <- colSums(full)
words <- words[,1]     
words <- as.vector(words)

## SVD results ###
d <- matrix(NA,ncol = 40,nrow = 20)   # put the results in a table
for (i in 1:20) {
  print(i)
  data <- full[as.vector(clusters)==i,]
  s <- colSums(data)
  data <- data[,s!=0]     # delete columns which are all 0 in it
  newwords <- words[s!=0]
  svd <- svd(data)
  values <- sort(abs(svd$v[,1]),decreasing = TRUE)[c(1:20)]
  print(values)
  d[,2*i-1] <- as.vector(values)
  keyword <- newwords[order(abs(svd$v[,1]),decreasing = TRUE)[c(1:20)]]
  print(keyword)
  d[,2*i] <- as.vector(keyword)
}
d <- as.data.frame(d)

### tf-idf results ####
d <- matrix(NA,ncol = 40,nrow = 20)
for (i in 1:20) {
  print(i)
  colsum <- colSums(full[as.vector(clusters)==i,])
  print(colsum[order(colsum,decreasing = TRUE)[c(1:20)]])
  d[,2*i-1] <- as.vector(colsum[order(colsum,decreasing = TRUE)[c(1:20)]])
  print(as.vector(words[order(colsum,decreasing = TRUE)[c(1:20)]]))
  d[,2*i] <- as.vector(words[order(colsum,decreasing = TRUE)[c(1:20)]])
}
d <- as.data.frame(d)
