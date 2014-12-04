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
#' @return list object (First list is Node, second list is Edge.) from apriori function
#' @examples
#' # Set support value 0.01
#' NodeEdge <- generateNodeEdge(sony.freq)
#' NodeEdge[[1]]  # Node
#' NodeEdge[[2]]  # Edge

generateNE <- function(HMDM.data, rules.minlen, rules.maxlen ,rules.supp=0.1, rules.conf=0.8){
  
  require(arules)
  require(stringr)
  
  freq.data.m <- as.matrix(HMDM.data)
  row.names(freq.data.m) <- 1:nrow(HMDM.data)
  
  transaction_m <- as(freq.data.m, 'transactions')
  # summary(transaction_m)
  rules.all <- apriori(transaction_m, parameter=list(minlen=rules.minlen, maxlen=rules.maxlen, supp=rules.supp, conf=rules.conf))
  # inspect(rules.all)
  
  #### Edge ####
  #Source
  lhs <- as(rules.all@lhs, "list")
  lhs.s <- as.character(lhs) 
  
  #Target
  rhs <- as(rules.all@rhs, "list")
  rhs.t <- as.character(rhs) 
  
  #Support
  sup <- (rules.all@quality)$support
  
  #Confidence
  con <- (rules.all@quality)$confidence
  
  #### Node ####
  label.tmp1 <- unique(lhs.s)
  label.tmp2 <- unique(rhs.t)
  label <- unique(c(label.tmp1, label.tmp2))
  
  id <- as.character(c(1:length(label)))
  
  Node <- data.frame(id=id, label=label, stringsAsFactors=FALSE)
  
  # Node Freq
  wordfreq <- rowSums(t(freq.data.m))
  names(wordfreq)
  word.freq <- data.frame(label=names(wordfreq), freq=wordfreq)
  Node.freq <- merge(word.freq, Node, by='label')
  Node.freq <- Node.freq[,3:2]
  Node.freq$id <- as.numeric(Node.freq$id)
  Node.freq <- Node.freq[ order(Node.freq$id),]
  Node.freq$id <- as.character(Node.freq$id)
  Node$freq <- Node.freq$freq
  Node$level1 <- unlist(lapply(1:nrow(Node), function(x) (str_split(Node$label, pattern='-'))[[x]][1]))
  
  #### Edge ####
  
  Edge <- data.frame(source=lhs.s, label=rhs.t, supp=sup, conf=con, stringsAsFactors=FALSE)
  
  Edge.1 <- merge(Edge, Node[,1:2], by="label")
  Edge.2 <- Edge.1[,2:5]
  colnames(Edge.2) <- c("label", 'supp','conf', "target")
  Edge.3 <- merge(Edge.2, Node[,1:2], by="label")
  Edge.4 <- Edge.3[,c(5,4,2,3)]
  colnames(Edge.4) <- c("source", "target",'supp','conf') # head(Edge.4)
  
  assign('Node', Node, envir=.GlobalEnv)
  assign('Edge', Edge.4, envir=.GlobalEnv)
  
  return(list(Node=Node, Edge=Edge.4))
  
}

