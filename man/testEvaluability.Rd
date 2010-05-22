\name{testEvaluability}
\alias{is.evaluable}
\title{Heuristically test expressions for evaluability...}
\usage{is.evaluable(x, envir=globalenv(), ...)
allEvaluable(l)
}
\description{Heuristically test expressions for evaluability}
\details{\code{is.evaluable}: \code{is.evaluable} tests if all components of the expression \code{x} exist in the
environment \code{envir}.
\code{allEvaluable} tests if all elements of the list \code{l} pass the
\code{is.evaluable} test.

}
\value{\code{is.evaluable}: True if the test was positive.

}
\arguments{\item{x}{An expression to test for evaluability.}
\item{l}{A list of expressions to test for evaluability.}
\item{envir}{The environment to perform the test in.}
\item{...}{Additional arguments for \code{\link{exists}}, which is used to test if
all components of \code{x} exist in \code{envir}.}
}
\alias{allEvaluable}

