\name{simplify}
\alias{simplify}
\title{Simplify a symbolic expression...}
\usage{simplify(x, algorithm=rulebasedSimplify, ...)
\method{simplify}{function}(x, algorithm=rulebasedSimplify, ...)
\method{simplify}{call}(x, algorithm=rulebasedSimplify, ...)
}
\description{Simplify a symbolic expression}
\details{\code{simplify}: Tries to transform a symbolic expression to an equivalent but simpler form.
\code{simplify} has methods for simplifying functions and calls.

}
\value{\code{simplify}: The simplified expression.

}
\arguments{\item{x}{The expression to simplify.}
\item{algorithm}{The algorithm to use for simplifying \code{x}. Defaults to
\code{\link{rulebasedSimplify}}.}
\item{...}{Additional arguments to \code{algorithm}.}
}
\alias{simplify.function}
\alias{simplify.call}

