\name{applyRulebase}
\alias{applyRulebase}
\title{Repeatedly apply a rulebase to an input object...}
\usage{applyRulebase(input, rulebase, matcher=patmatch, action=substCall)
}
\description{Repeatedly apply a rulebase to an input object}
\details{Translate an input object by applying a rule base repeatedly, until the result does
not change anymore. \code{matcher} is tried on each rule in \code{rulebase} in order,
until a matching rule is found. \code{matcher} must return a binding object or \code{NA}.
When a rule matches, \code{action} is applied to the binding object and the rule's
consquence pattern, giving a result expression. After that, the rule base is applied
to the result expression again, if the result differs from \code{input}.
The \code{\link{simplify}} function gives a practical example for using
\code{applyRulebase} concerning the simplification of symbolic expressions.}
\value{The result of applying the \code{rulebase}.}
\arguments{\item{input}{The input object to translate.}
\item{rulebase}{The rulebase to use for translation.}
\item{matcher}{The function used to match the antecedens of each rule. Defaults to
\code{\link{patmatch}}.}
\item{action}{The action to apply when a rule matches.}
}

