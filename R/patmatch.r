## patmatch.r
##   - Pattern matching with predicates
##
## Rrules - a simple rule based translator for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Pattern matching with predicates
##'
##' \code{patmatch} tries to match an input expression against a pattern expression that may
##' contain pattern variables and pattern predicates. The function returns a (possibly empty)
##' list of pattern variable bindings if the pattern matches the input, or \code{fail} if the
##' pattern does not match the input.
##'
##' A pattern variable is simply a \code{\link{name}} starting with \code{"v."}. An unbound
##' pattern variable will match any input. It is bound on the first match. A bound pattern
##' variable matches the input if its binding matches the input. A pattern predicate is a
##' call of the form \code{v.is(var, pred)}, where \code{var} is a pattern variable and
##' \code{pred} is a one-argument function returning a single logical value. A pattern
##' predicate behaves as if it where the pattern variable \code{var}, but only matches input
##' if calling \code{pred} on the input returns true.
##'
##' \code{fail} ist just defined as \code{NA} in this version, but this might change. Use
##' \code{is.fail} to tests if a value is the "fail" value.
##' \code{emptyBindings} represents an empty bindings object. Use \code{has.binding},
##' \code{get.binding}, and \code{add.binding} to work with binding objects.
##' \code{substCall} substitutes all pattern variables present in a call with their bindings
##' given as a binding object.
##'
##' @param pattern The pattern to match \code{input} againts.
##' @param input The input expression to match against.
##' @param bindings Existing pattern variable bindings.
##' @return A list of pattern variable bindings, or \code{fail} if the pattern
##' did not match the input.
##'
##' @examples
##' patmatch(quote(v.X * (v.Y / v.X)), quote((2 + a) * (5 / (2 + a))))
##' patmatch(quote(v.is(v.X, is.numeric) + v.is(v.X, is.numeric)), quote(2 + 2))
##' 
##' @rdname patternMatching
##' @export
patmatch <- function(pattern, input, bindings = emptyBindings) {
  if (is.fail(bindings))
    fail
  else if (is.patvar(pattern))
    matchPatvar(pattern, input, bindings)
  else if (identical(pattern, input))
    bindings
  else if (is.patpred(pattern))
    matchPatpred(pattern, input, bindings)
  else if (is.call(pattern) && is.call(input))
    Recall(callArgs(pattern), callArgs(input),
           Recall(callName(pattern), callName(input), bindings))
  else if (is.list(pattern) && is.list(input) &&
           !is.empty(pattern) && !is.empty(input))
    Recall(rest(pattern), rest(input),
           Recall(first(pattern), first(input), bindings))
  else
    fail
}

matchPatvar <- function(patvar, input, bindings) {
  binding <- get.binding(bindings, patvar)
  if (is.null(binding))
    add.binding(bindings, patvar, input)
  else if (identical(binding, input))
    bindings
  else
    fail
}

matchPatpred <- function(patpred, input, bindings) {
  patpredArgs <- callArgs(patpred)
  patpredVar <- patpredArgs[[1]]
  patpredName <- patpredArgs[[2]]
  varBindings <- matchPatvar(patpredVar, input, bindings)
  if (eval(bquote(.(patpredName)(quote(.(input)))), envir = globalenv()))
    varBindings
  else
    fail
}

##' @rdname patternMatching
##' @export
fail <- NA

##' @rdname patternMatching
##' @export
is.fail <- function(bindings) !is.emptyBindings(bindings) && identical(NA, bindings)

##' @rdname patternMatching
##' @export
emptyBindings = list()

##' @rdname patternMatching
##' @export
is.emptyBindings <- function(bindings) length(bindings) == 0

##' @rdname patternMatching
##' @param patvar name of pattern
##' @export
get.binding <- function(bindings, patvar) bindings[[as.character(patvar)]]

##' @rdname patternMatching
##' @export
has.binding <- function(bindings, patvar) !is.null(get.binding(bindings, patvar))

##' @rdname patternMatching
##' @param value value to bind
##' @export
add.binding <- function(bindings, patvar, value) {
  bindings[[as.character(patvar)]] <- value
  bindings
}

##' @rdname patternMatching
##' @param x Value for bindings
##' @export
substCall <- function(bindings, x) {
  xIsPatvar <- is.patvar(x)
  if (xIsPatvar && has.binding(bindings, x))
    get.binding(bindings, x)
  else if (is.call(x))
    as.call(Recall(bindings, as.list(x)))
  else if (is.atom(x) && is.list(x)) # R does not distinguish between atoms and 1-element lists
    Recall(bindings, first(x))
  else if (is.composite(x))
    c(Recall(bindings, first(x)), Recall(bindings, rest(x)))
  else
    x
}

patvarPrefix <- "v."
is.patvar <- function(x)
  is.name(x) &&
  nchar(as.character(x)) > 2 &&
  identical(patvarPrefix, substr(as.character(x), 1, nchar(patvarPrefix)))

patpredPrefix <- "v.is"
is.patpred <- function(x)
  is.call(x) &&
  identical(as.character(callName(x)), patpredPrefix) &&
  length(x) == 3

