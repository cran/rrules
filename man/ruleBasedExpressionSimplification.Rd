\name{ruleBasedExpressionSimplification}
\alias{rulebasedSimplify}
\title{Simplifies an expression using a rule based simplification algorithm.}
\usage{rulebasedSimplify(x, rulebase=basicSimplificationRules, evaluate=TRUE)
}
\description{Simplifies an expression using a rule based simplification algorithm.}
\details{\code{rulebasedSimplify}: \code{arithmeticSimplificationRules} contains rules for simplifying arithmetic
expressions, \code{logarithmSimplificationRules} contains rules for simplifying
expressions with logarithms and exponents, \code{trigonometricSimplificationRules}
contains rules for simplifying expressions with sines and cosines.
\code{basicSimplificationRules} contains all of the above rules.

}
\value{\code{rulebasedSimplify}: The simplified expresssion.

}
\arguments{\item{x}{The expression to simplify.}
\item{rulebase}{The rulebase to use for simplification. Defaults to
\code{\link{basicSimplificationRules}}.}
\item{evaluate}{Whether to evaluate constant subexpressions during simplification.}
}
\alias{arithmeticSimplificationRules}
\alias{logarithmSimplificationRules}
\alias{trigonometricSimplificationRules}
\alias{basicSimplificationRules}

