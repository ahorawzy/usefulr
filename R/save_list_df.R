#' Write dataframes saved in a list
#'
#' \code{save_list_df} writes dataframes saved in a list into certain folder in batch.
#'
#' @param objectlist The list contains several dataframes as its elements.
#' @param path A string of path, ended by '\\\' in windows, like 'D:\\\'
#' @return Write csv files in the certain path and return NULL.
#'
#' @example
#' A = list(df1 = data.frame(a = 1:3,b = letters[1:3]), df2 = data.frame(a = 1:10, b = LETTERS[1:10]))
#' save_list_df(A, 'D:\\')
#'
#' @export
save_list_df <- function(objectlist, path) {
    for (i in 1:length(objectlist)) {
        address <- paste(c(path, format(names(objectlist)[i]), ".csv"), collapse = "")
        write.csv(objectlist[[i]], file = address, na = "", row.names = F)
    }
}
