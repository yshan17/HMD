#' generateNE
#' 
#' @description Create Node and Edge object from apriroi function
#' 
#' @param HMDM.data data by generateHMDM function
#' @param rules.supp a numeric value for the minimal support of an item set (default : 0.1)
#' @param rules.conf a numeric value for the minimal confidence of rules/association hyperedges (default : 0.8)
#' @param rules.minlen an integer value for the minimal number of items per item set (default : 2)
#' @param rules.maxlen an integer value for the maximal number of items per item set (default : 2)
#' 
#' @return Node and Edge object(data.frame) from apriori function
#' @examples
#' # Set support value 0.01
#' generateNE(HMDM.data=data.freq, rules.supp=0.01)
generateNE <- function(HMDM.data, rules.supp=0.1, rules.conf=0.8, rules.minlen=2, rules.maxlen=2){
  
  require(arules)
  
  freq.data.m <- as.matrix(HMDM.data)
  row.names(freq.data.m) <- 1:nrow(HMDM.data)
  
  # Association : apriori function in 'arules' package
  transaction_m <- as(freq.data.m, 'transactions')
  rules.all <- apriori(transaction_m, parameter=list(minlen=rules.minlen, maxlen=rules.maxlen, supp=rules.supp, conf=rules.conf))
  # inspect(rules.all)
  
  # Network Graph
  
  ## 1. Edge - 1
  # Source
  lhs <- as(rules.all@lhs, "list")
  lhs.s <- as.character(lhs) 
  # Target
  rhs <- as(rules.all@rhs, "list")
  rhs.t <- as.character(rhs) 
  
  ## 2. Node
  # label
  label.tmp1 <- unique(lhs.s)
  label.tmp2 <- unique(rhs.t)
  label <- unique(c(label.tmp1, label.tmp2))
  # id
  id <- as.character(c(1:length(label)))
  Node <- data.frame(id=id, label=label, stringsAsFactors=FALSE)
  
  # Freq of Node
  wordfreq <- rowSums(t(freq.data.m))
  word.freq <- data.frame(label=names(wordfreq), freq=wordfreq)
  Node.freq <- merge(word.freq, Node, by='label')
  Node.freq <- Node.freq[,3:2]
  Node.freq$id <- as.numeric(Node.freq$id)
  Node.freq <- Node.freq[ order(Node.freq$id),]
  Node.freq$id <- as.character(Node.freq$id)
  Node$freq <- Node.freq$freq
  
  ## 3. Edge - 2
  
  Edge <- data.frame(source=lhs.s, label=rhs.t, stringsAsFactors=FALSE)
  
  Edge.1 <- merge(Edge, Node, by="label")
  Edge.2 <- Edge.1[,2:3]
  colnames(Edge.2) <- c("label", "target")
  Edge.3 <- merge(Edge.2, Node, by="label")
  Edge.4 <- Edge.3[,3:2]
  colnames(Edge.4) <- c("source", "target")
  
  assign('Node', Node, envir=.GlobalEnv)
  assign('Edge', Edge.4, envir=.GlobalEnv)
}
