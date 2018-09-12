#' srt2subtitle
#'
#' This function can convert srt to subtitle of txt format
#'
#' @param infilepath The infilepath like "D://data//BBC.srt".
#' @param outfilepath The outfilepath like "D://data//BBC.txt"
#'
#' @return write file into certain path
#' @export
#'
srt2subtitle <- function(infilepath,outfilepath){
  srt <- readLines(infilepath)
  spaceline <- which(srt == "")
  words <- srt[-c(1,2,spaceline,spaceline+1,spaceline+2)]
  article <- paste(words,collapse = " ")
  write.table(article,file=paste(c(outfilepath)))
}
