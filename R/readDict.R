#' readDict
#' 
#' @description Read a dictionary csv file. Convert data fram to HMD class
#' @param dict.path folder path of dictionary file
#' @return HMD class object
readDict <- function(dict.path, ...){
  
  ## Description : 
  #
  ## Arguments
  # dict.path : 
  
  tmp <- read.table(file=dict.path, sep=',', header=T)
  
  ## Create a 'hmd' class
  setClass('hmd', representation(x='data.frame'))
  tmp <- new('hmd', x=tmp)
  
  ## Set Methods
  # (1) Set a 'length' Method
  setMethod('length','hmd', 
            function(object){
              l <- length(object@x); 
              return(l);})
  # (2) Set a 'nrow' Method
  setMethod('nrow','hmd',
            function(object){
              n <- nrow(object@x);
              return(n);})
  # (3) Set a 'summary' Method
  summary.hmd <- function(object){
    
    ## Description :
    #
    ## Attributes 
    # data:
    
    # 단어의 수 
    s1 <- nrow(object@x)
    
    # level의 수
    s2 <- length(object@x)-1
    
    # unique한 level값
    s3 <- lapply(2:length(object@x), function(i) unique(object@x[,i])[!unique(object@x[,i]) %in% ''])
    names(s3) <- names(object@x)[-1]
    
    tt <- unlist(lapply(1:length(s3), function(i) length(s3[[i]])))
    
    s4 <- lapply(1:length(tt), function(i) c(s3[[i]], rep('',max(tt)-tt[i])))
    s4 <- ldply(s4)
    row.names(s4) <- names(object@x)[-1]
    colnames(s4) <- 1:length(s4)
    
    # 각 level별 항목 수
    s5 <- numeric(length(s3))
    for(i in 1:length(s3)){
      s5[i] <- length(s3[[i]])
    }
    s5 <- t(as.data.frame(s5))
    colnames(s5) <- names(object@x)[-1]
    row.names(s5) <- NULL
    
    # 생성날짜
    s6 <- format(Sys.time(), "%Y-%m-%d")
    
    # 전체를 하나의 list로 생성
    final <- list('number of words'=s1, 'number of levels'=s2, 'value of levels'=s4, 'number of value'=s5, time=s6)
    
    return(final)
    
  }
  setMethod("summary", "hmd", summary.hmd)
  
  # (4) Set a 'plot' Method
  # plot.hmd <- function(object){}  
  # setMethod("plot", "hmd", plot.hmd)
  
  
  # (5) Set a 'setAs' Method
  setAs('hmd','data.frame', function(from) from@x)
  showMethods("coerce", classes = "hmd")
  
  ## Attributes
  attr(tmp, 'number of level') <- length(tmp)-1
  attr(tmp, 'number of words') <- nrow(tmp)
  
  return(tmp)
  
}