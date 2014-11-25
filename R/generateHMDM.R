#' generateHMDM
#' 
#' @description Generate HMDM(Hierarchical Multi-Dictionary term Matrix) function
#' @param corpus object of class Corpus
#' @param dict object of class HMD
#' @return data frame (row : Document , column : Dictionary Term)
generateHMDM <- function(corpus, dict, ...){
  
  ## Description : Corpus형태의 데이터와 사용자 Heirarchical multi-dictionary를 이용하여, 
  #                HMDM(Hierarchical Multi-Dictionary Matrix)를 구축하는 함수.
  #
  ## Arguments
  # corpus : Corpus class를 가지는 데이터
  # dict :  첫 번째 열이 단어로 이루어진 데이터프레임 형식의 사용자 dictionary.
  
  require(RWeka)
  BigramTokenizer <- function(x, min=1, max=10) NGramTokenizer(x, Weka_control(min = min, max=max))
  ## Description : tokenize function in 'RWeka' package
  #
  ## Argrument 
  # x : tokenize할 대상으로 data.frame이나 vector형식의 데이터
  # min : 최소 어절의 수. default값은 1로 띄어쓰기가 존재하지 않는 형식
  # max : 최대 어절의 수. default값은 10으로 총 11번의 띄어쓰기가 존재하는 형식
  
  
  # dictionary ordering
  dict <- as(dict, 'data.frame')
  dict <- dict[order(dict[,1]),]
  
  # linked levels
  level <- c()                                  
  for(i in 1:nrow(dict)){ 
    term <- dict[i,-1]                            # vec 부분 제외
    remove.white <- term[! term %in% '']          # Whitespace제거
    level[i] <- paste(remove.white, collapse='-')
  }
  vec.levels <- data.frame(Vec=as.character(dict[,1]), Levels=level)
  level.dict <- unique(data.frame(dict[,2:ncol(dict)], term=level))
  #
  #assign('level.dict', level.dict, pos=1)
  
  # DocumentTermMatrix 생성
  # rownames은 같은 이름으로 지정이 불가하여 DTM을 사용
  tmp.dtm <- DocumentTermMatrix(corpus, control=list(tokenize=BigramTokenizer, dictionary=vec.levels$Vec))
  tmp.m <- as.matrix(tmp.dtm)                               # DTM -> Matrix
  tmp.d <- as.data.frame(t(tmp.m))
  colnames(tmp.d) <- paste('Doc', colnames(tmp.d), sep='_') # Document부분을 1,2,...,n -> Doc_1, Doc_2,..., Doc_n으로 변경
  
  # 같은 Attr을 갖는 단어끼리 묶기
  tmp.d$name <- level
  tmp.count <- ddply(tmp.d, .(name), numcolwise(sum))
  rownames(tmp.count) <- tmp.count$name
  tmp.count <- tmp.count[,-1]
  tmp.count <- as.data.frame(t(tmp.count))
  
  # Attributes 추가
  attr(tmp.count,'corpus') <- paste(attributes(corpus)$data.name, '.corpus', sep='')
  attr(tmp.count, 'data.name') <- attributes(corpus)$data.name
  
  return(tmp.count)
}