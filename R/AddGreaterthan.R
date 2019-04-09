#' @export
AddGreaterthan <- function(filename,encodestyle){
  file <- readLines(filename,encoding=encodestyle)
  noemptyindex <- which(nchar(file)!=0)
  file[noemptyindex] <- paste(">",file[noemptyindex])
  writeLines(file,filename)
}
