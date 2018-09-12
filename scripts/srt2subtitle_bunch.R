infiles = vapply(files,function(x) paste(c("D:\\data\\字幕\\",x),collapse=""),FUN.VALUE=character(1))
outfiles = paste("D:\\data\\字幕\\",1:20,".txt",sep="")
