#' @title A print method for rnames
#' @name print.rnames
#' @description A customized printed display for rnames output
#' @param x A rnames object
#' @param ... Undocumented
#' @returns No return, custom print method for rnames objects.
#' @export
print.rnames <- function(x, ...) {
    for (v in 1:length(names(x))) {
        cat(dir_display(x[[v]]), "\n")
    }
}

dir_display <- function(vec) {
    str <- ""
    for (j in 1:length(vec)) {
        str <- paste0(str, "/", vec[j])
    }
    return(str)
}