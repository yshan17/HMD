#' readData
#' 
#' @description Use data folder path, read Data function
#' @param data.name data name to be used. At least one vector
#' @param folder.path Folder path of data
#' @param pattern Pattern of load data
#' @return data file
#' @export data file
readData <- function(data.name, folder.path, pattern='*.csv'){
  
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
