\name{patternMatching}
\alias{patmatch}
\title{Pattern matching with predicates...}
\usage{patmatch(pattern, input, bindings=emptyBindings)
is.fail(bindings)
is.emptyBindings(bindings)
get.binding(bindings, patvar)
has.binding(bindings, patvar)
add.binding(bindings, patvar, value)
substCall(bindings, x)
}
\description{Pattern matching with predicates}
\details{\code{patmatch}: \code{patmatch} tries to match an input expression against a pattern expression that may
contain pattern variables and pattern predicates. The function returns a (possibly empty)
list of pattern variable bindings if the pattern matches the input, or \code{fail} if the
pattern does not match the input.

A pattern variable is simply a \code{\link{name}} starting with \code{"v."}. An unbound
pattern variable will match any input. It is bound on the first match. A bound pattern
variable matches the input if its binding matches the input. A pattern predicate is a
call of the form \code{v.is(var, pred)}, where \code{var} is a pattern variable and
\code{pred} is a one-argument function returning a single logical value. A pattern
predicate behaves as if it where the pattern variable \code{var}, but only matches input
if calling \code{pred} on the input returns true.

\code{fail} ist just defined as \code{NA} in this version, but this might change. Use
\code{is.fail} to tests if a value is the "fail" value.
\code{emptyBindings} represents an empty bindings object. Use \code{has.binding},
\code{get.binding}, and \code{add.binding} to work with binding objects.
\code{substCall} substitutes all pattern variables present in a call with their bindings
given as a binding object.

}
\value{\code{patmatch}: A list of pattern variable bindings, or \code{fail} if the pattern
did not match the input.

}
\arguments{\item{pattern}{The pattern to match \code{input} againts.}
\item{input}{The input expression to match against.}
\item{bindings}{Existing pattern variable bindings.}
\item{patvar}{name of pattern}
\item{value}{value to bind}
\item{x}{Value for bindings}
}
\examples{patmatch(quote(v.X * (v.Y / v.X)), quote((2 + a) * (5 / (2 + a))))
patmatch(quote(v.is(v.X, is.numeric) + v.is(v.X, is.numeric)), quote(2 + 2))}
\alias{fail}
\alias{is.fail}
\alias{emptyBindings}
\alias{is.emptyBindings}
\alias{get.binding}
\alias{has.binding}
\alias{add.binding}
\alias{substCall}

