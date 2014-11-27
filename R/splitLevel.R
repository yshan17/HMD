#' splitLevel
#' 
#' @description HMDM(Hierarchical Multi-Dictionary term Matrix) level split function
#' @param HMDM.data data by generateHMDM function
#' @return splited level
#' @examples
#' # 
#' sony.level <- split(HMDM.data=data.freq)
splitLevel <- function(HMDM.data){
  
  ## Description : 
  #
  ## Arguments
  # freq.data : Term
  # level 
  
  freq.data.t <- as.data.frame(t(HMDM.data))
  freq.data.t[freq.data.t >=1] <- 1
  freq.data.t$term <- row.names(freq.data.t)
  freq.data.term <- merge(freq.data.t, level.dict, by='term')
  freq.data.term <- freq.data.term[,-1]
  
  freq.data.term$total <- apply(freq.data.term[,1:nrow(HMDM.data)], 1, sum)
  
  #attr(freq.data.term, 'level.count') <- 
  #attr(freq.data.term, 'level.docs.name') <- 
  #attr(freq.data.term, 'level.docs.num') <- 
  #attr(freq.data.term, 'corpus') <- 
  
  return(freq.data.term)
}