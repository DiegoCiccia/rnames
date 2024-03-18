#' Recursive function to get names in nested lists
#' @md
#' @description Recursive display of names and paths of all the items nested within sublists of a list object.
#' @param obj A list to be traversed.
#' @param ignore A list of sublists to exclude from binary tree traversal. The program will report the ignored sublists as end-points. This option is normally suggested for very deep sublists that may cause recursion errors.
#' @param ... Undocumented
#' @section Description:
#' The \code{rnames()} function recursively runs \code{names()} on a list object and returns a list with the names and paths of all the end items. 
#' The paths are arrays cointaining all the sublists that need to be accessed in order to retrieve the corresponding item. 
#' The built-in \code{names()} function only returns the names of the objects on the top-most layer of the list. 
#' Therefore, all the other subobjects can only be browsed by reiterating \code{names()} on their parent object. 
#' Instead, \code{rnames()} returns the name and paths of all the end-points of any list. 
#' This allows the user to browse all the non-list elements of a nested list without having to manually inspect each sublist. 
#' 
#' The program may halt if the recursion goes too deep.
#' With the \code{ignore} option, the user can halt the execution of the traversal algorithm beyond certain specified nodes. 
#' In this way, the program is prevented from exceeding recursion limits.
#' Nodes can be referenced by their object name (e.g., the output of \code{names()} on their parent object). 
#' Notice that the nodes specified in the \code{ignore} argument will be included in the output.
#' The function stops from traversing the nodes nested inside those specified in the \code{ignore} option.
#' 
#' By definition, a list object can be very complex to visualize due to the presence of sublists. 
#' The key idea is that a list object and its subobjects can be represented as the root and leaves of a tree graph, respectively.
#' Sublists form subtrees which can be inspected for subleaves. 
#' An object having no sub-objects is the end-point of a list, since it is not a list itself.
#' At the same time, data and other objects are stored in end-points.
#' Thus, if these objects are stored in nested lists, it is surely convenient to traverse the list and show all the subobjects at once.
#' @returns A list with entries corresponding to the end-points of the traversed list. The name of each element is the name of the end-point, while the item is a vector of the sublists that lead to the end-point.
#' @examples
#' deep_list <- list(
#'     A = list(B = 2, C = 3, D = list(L = "A", M = "B", N = "C")),
#'     B = list(V1 = 2, V2 = 3),
#'     C = list(V1 = 2, V2 = 3),
#'     D = 4)
#' print(rnames(deep_list, ignore = c("D")))
#' @export
rnames <- function(obj, ignore, ...) {
    UseMethod("rnames")
}

#' @title General rnames method for lists
#' @name rnames.list
#' @description General rnames method for generic lists.
#' @param obj A list
#' @param ignore A set of sublists to be ignored
#' @param ... Undocumented
#' @returns A list with rname class.
#' @export
rnames.list <- function(obj = obj, ignore = c(), ...) {
    out <- rec_search(obj = obj, text = NULL, ignore = ignore)
    class(out) <- "rnames"
    return(out)
}

rec_search <- function(obj, text, out = list(), ignore) {
    for (v in names(obj)) {
        text <- c(text, v)
        if (!is.null(names(obj[[v]])) & !(v %in% ignore)) {
            out <- rec_search(obj[[v]], text, out, ignore)
        } else {
            out <- append(out, list(text))
            names(out)[length(out)] <- v
        }
        text <- text[-length(text)]
    }
    return(out)
}