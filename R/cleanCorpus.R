#' cleanCorpus
#' 
#' @description Convert data data frame to Corpus
#' @param data data for cleansing, data frame or character vector
#' @param lower a logical value indicating whether the character vectors translate characters in particular from upper to lower case or vice versa. (default : T)
#' @param removeP a logical value indicating whether the character vectors remove all punctuations. (default : T)
#' @param removeWS a logical value indicating whether the character vectors remove all white spaces. (default : F)
#' @param multipattern a character vector indicating the character vectors remove pattern. (default : NULL)
#' @param stem a logical value indicating whether the character vectors stemming characters. (default : F)
#' @param removeN a logical value indicating whether the character vectors remove all numbers. (default : T)
#' @export data file
#' @examples
#' 
#' stopwords=c('from','and','for','was','had','ncp','will','its','required','unit','require')
#' cleanCorpus(VoC.text, stem=T, mystopwords=stopwords)
cleanCorpus <- function(data, lower=T, removeP=T, removeWS=F, removeSW=F, multipattern=NULL, stem=F, removeN=F){
  
  ## Description  : 데이터를 원하는 방법으로 전처리하여 Corpus형태의 데이터로 반환해주는 함수
  #
  ## Arguments
  # data         : data.frame이나 vector형식의 데이터로 Corpus로 변환할 데이터
  # lower        : (영문에서) 대문자 -> 소문자 변환의 실행여부
  # removeP      : 문장 부호 제거여부
  # removeWS     : 문장의 빈 공간 제거여부
  # removeSW    : (영문에서) 기본 단어 제거여부
  # multipattern : 제거하고자 하는 패턴
  # stem         : (영문에서) 단어에 대한 stemming과정 실행여부
  # removeN      : 숫자 제거여부
  
  require(tm)
  require(SnowballC)
  
  cat('Convert Data to Corpus class......')
  if(is.data.frame(data)==TRUE){
    my.corpus <- Corpus(DataframeSource(data))}
  else{
    if(is.character(data)==TRUE){
      my.corpus <- Corpus(VectorSource(data))}
    else{stop()}
  }
  cat('Done\n')
  
  # 텍스트 마이닝의 일반적인 데이터 전처리 과정
  cat('processing 1 : tolower............')
  if(lower==T) { my.corpus <- tm_map(my.corpus, content_transformer(tolower))}               # .....대문자 -> 소문자 변환
  cat('\n')
  
  cat('processing 2 : remove Punctuation..')
  if(removeP==T) { my.corpus <- tm_map(my.corpus, content_transformer(removePunctuation))}   # .....문장부호, 구두점 제거
  cat('\n')
  
  cat('processing 3 : remove stripWhitespace..')
  if(removeWS==T) { my.corpus <- tm_map(my.corpus, content_transformer(stripWhitespace))}   # .....문장부호, 구두점 제거
  cat('\n')
  
  cat('processing 4 : remove Words........')
  if(removeSW==T) { my.corpus <- tm_map(my.corpus, removeWords, stopwords('en'))}     # .....원하는 pattern 제거 
  cat('\n')  
  
  cat('processing 5 : remove Enter........')
  f <- content_transformer(function(x) {gsub('\n','',x)})
  my.corpus <- tm_map(my.corpus, f)
  cat('\n')  
  
  cat('processing 6 : remove multiPattern.......')
  mgsub <- function(multi.pattern, replacement,data){
    data <- as.character(data)
    for(i in 1:length(multi.pattern)){
      data <- gsub(multi.pattern[i], replacement, data)
    }
    return(data)
  }
  if(length(multipattern)!=0) { 
    my.corpus <- tm_map(my.corpus, content_transformer(mgsub), multi.pattern=multipattern, replacement='')
    attr(my.corpus, 'remove.pattern') <- multi.pattern}     # .....원하는 pattern을 multi로 제거 
  cat('\n')  
  
  cat('processing 7 : stemDocument.......')
  if (stem == T) { 
    stemming.func <- function(Corpus.data){
      
      require(tm)
      
      tmp.1 <- list()
      for(i in 1:length(Corpus.data)){ tmp.1[[i]] <- unlist(str_split(as.character(content(Corpus.data)[[i]]), pattern=' ')) }
      corpus.length <- unlist(lapply(1:length(tmp.1), function(i) {length(tmp.1[[i]])}))
      corpus.length <- cumsum(corpus.length)+1
      tmp.1 <- unlist(tmp.1)
      tmp.1 <- tmp.1[!tmp.1 %in% '']
      corpus.2 <- Corpus(VectorSource(tmp.1))
      corpus.2 <- tm_map(corpus.2, stemDocument)
      corpus.2 <- tm_map(corpus.2, content_transformer(stemCompletion), dictionary=tmp.1, type='prevalent')
      tmp.2 <- list()
      for(i in 1:length(corpus.2)){ tmp.2[[i]] <- unlist(str_split(as.character(content(corpus.2)[[i]]), pattern=' ')) }
      tmp.2 <- unlist(tmp.2)
      splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% (pos))))
      split.list <- splitAt(tmp.2, corpus.length)
      split.list <- lapply(1:length(split.list), function(i) paste(split.list[[i]],collapse=' '))
      split.list <- as.character(split.list)
      assign('Stem.Data', split.list, envir=.GlobalEnv)
    }
    stemming.func(my.corpus)
    my.corpus <- Corpus(VectorSource(Stem.Data))
  }
  cat('\n')
  
  cat('processing 8 : removeNumbers......')
  if(removeN==T){ my.corpus <- tm_map(my.corpus, removeNumbers)}                             # ..... 숫자 제거
  cat('\n')
  
  cat('Out Corpus data...................')
  
  ## Attributes
  attr(my.corpus, 'data.name') <- attributes(data)$data.name
  attr(my.corpus, 'number.docs') <- length(data)
  attr(my.corpus, 'docs.name') <- attributes(data)$docs.name
  
  cat('\n')
  
  return(my.corpus)
  
}