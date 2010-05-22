\name{rulesRulebases}
\alias{rule}
\title{Rules and Rule Bases...}
\usage{rule(ruleif, rulethen)
rulebase(..., list)
print.rulebase(x, ...)
}
\description{Rules and Rule Bases}
\details{\code{rule}: \code{rule} creates a new rule from an antecedens pattern and a consequence pattern.
\code{rulebase} creates a new rulebase from one or more rules.
Rule bases can be combined with \code{combineRulebase}.
See \code{\link{applyRulebase}} for a typical application of rules and rule bases.

}
\value{\code{rule}: A new rule object.

\code{rulebase}: A new rulebase object.


}
\arguments{\item{ruleif}{The antecedens of a rule, given as a pattern.}
\item{rulethen}{The consquence of a rule, given also given as a pattern.}
\item{x}{The rule or rulebase to print.}
\item{...}{Rules}
\item{list}{a list of rules}
}
\alias{rulebase}
\alias{print.rulebase}

