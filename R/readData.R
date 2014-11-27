#' readData
#' 
#' @description Use data folder path, read Data function
#' @param data.name data name to be used. At least one vector
#' @param folder.path Folder path of data
#' @param pattern Pattern of load data
#' @return data file
#' @export data file
#' @examples 
#' # Two csv files exists 'C:/User/USER/Desktop/data' path
#' # One data is 'data1', the other is 'data2'
#' 
#' readData(data.name=c('data1','data2'), folder.path='C:/User/USER/Desktop/data', pattern='*.csv')
readData <- function(data.name, folder.path, pattern='*.csv'){
  
  ## Description : 특정 경로에서 특정pattern을 가지는 파일을 모두 읽어들이는 함수
  #
  ## Arguments
  # data.name : 저장될 데이터의 이름 지정 (path안의 파일 갯수와 같음)
  # folder.path : 데이터를 불러올 폴더의 경로 
  # pattern : 불러올 데이터의 패턴 지정
  
  require(stringr)
  
  cat('Loading file name...\n')
  data.files <- list.files(path=folder.path, pattern=pattern)
  no.data <- length(data.files)
  Data <- character()
  for (i in 1:no.data){
    cat('Loading file' ,i, '/', no.data, '.....\n')
    data.file.path <- paste(folder.path, data.files[i], sep='/')
    if(str_sub(data.file.path,-3,-1)=='csv'){
      data <- read.csv(file=data.file.path, sep=',', stringsAsFactors=F, header=T)}
    else {
      data <- readLines(file(data.file.path, encoding='EUC-KR'))
      data <- paste(data, collapse=" ")              
    }
    attr(data, 'data.name') <- data.name[i] # ....................
    attr(data, 'folder.path') <- folder.path # .................
    attr(data, 'number.docs') <- nrow(data) # .....................
    attr(data, 'docs.name') <- data.files[i] # ....................
    assign(data.name[i], data, envir=.GlobalEnv)
  }
  cat('Done!\n')
}

