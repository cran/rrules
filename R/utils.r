## utils.r
##   - Utility functions for Rrules
##
## Rrules - a simple rule based translator for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Get the name or argument list of a call
##'
##' \code{callName} returns the name of a \code{\link{call}}.
##' \code{callArgs} returns the argument list of a call.
##'
##' @param call The call to return the name or argument list for.
##' @return The name or argument list of a call.
##' @rdname callNameArgs
callName <- function(call) (as.list(call))[[1]]

##' @rdname callNameArgs
callArgs <- function(call) rest(as.list(call))

##' Functions for Lisp-like list processing
##'
##' Simple wrapper functions that allow Lisp-like list processing in R:
##' \code{first} to \code{fifth} return the first to fifth element of the list \code{x}.
##' \code{rest} returns all but the first element of the list \code{x}.
##' \code{is.empty} returns \code{TRUE} iff the list \code{x} is of length 0.
##' \code{is.atom} returns \code{TRUE} iff the list \code{x} is of length 1.
##' \code{is.composite} returns \code{TRUE} iff the list \code{x} is of length > 1.
##'
##' @param x A list or vector.
##'
##' @rdname lispLists
first <- function(x) x[[1]]

##' @rdname lispLists
rest <- function(x) x[-1]

##' @rdname lispLists
second <- function(x) x[[2]]

##' @rdname lispLists
third <- function(x) x[[3]]

##' @rdname lispLists
fourth <- function(x) x[[4]]

##' @rdname lispLists
fifth <- function(x) x[[5]]

##' @rdname lispLists
is.empty <- function(x) length(x) == 0

##' @rdname lispLists
is.atom <- function(x) length(x) == 1

##' @rdname lispLists
is.composite <- function(x) length(x) > 1

##' Test if an object is a symbolic expression
##'
##' @param x The object to test.
##' @return True if \code{x} is not numeric.
is.symbolic <- function(x) !is.numeric(x)

##' Heuristically test expressions for evaluability
##'
##' \code{is.evaluable} tests if all components of the expression \code{x} exist in the
##' environment \code{envir}.
##' \code{allEvaluable} tests if all elements of the list \code{l} pass the
##' \code{is.evaluable} test.
##'
##' @param x An expression to test for evaluability.
##' @param l A list of expressions to test for evaluability.
##' @param envir The environment to perform the test in.
##' @param ... Additional arguments for \code{\link{exists}}, which is used to test if
##' all components of \code{x} exist in \code{envir}.
##' @return True if the test was positive.
##'
##' @rdname testEvaluability
is.evaluable <- function(x, envir = globalenv(), ...) {
  if (is.call(x))
    is.evaluable(callName(x)) && allEvaluable(callArgs(x))
  else if (is.name(x))
    exists(as.character(x), envir = envir, ...)
  else if (is.atomic(x))
    TRUE
  else
    FALSE
}

##' @rdname testEvaluability
allEvaluable <- function(l) {
  if (is.atom(l))
    is.evaluable(first(l))
  else
    is.evaluable(first(l)) && allEvaluable(rest(l))
}
