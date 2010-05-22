## rules.r
##   - S3 classes for rules and rulebases
##
## Rrules - a simple rule based translator for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Rules and Rule Bases
##'
##' \code{rule} creates a new rule from an antecedens pattern and a consequence pattern.
##' \code{rulebase} creates a new rulebase from one or more rules.
##' Rule bases can be combined with \code{combineRulebase}.
##' See \code{\link{applyRulebase}} for a typical application of rules and rule bases.
##'
##' @param ruleif The antecedens of a rule, given as a pattern.
##' @param rulethen The consquence of a rule, given also given as a pattern.
##' @param x The rule or rulebase to print.
##' @return A new rule object.
##'
##' @rdname rulesRulebases
##' @export
rule <- function(ruleif, rulethen) {
  newRule <- list(ruleIf = substitute(ruleif), ruleThen = substitute(rulethen)) # TODO
  class(newRule) <- c("rule", "list")
  newRule
}

##' @rdname rulesRulebases
##' @param ... Rules
##' @param list a list of rules
##' @return A new rulebase object.
##' @export
rulebase <- function(..., list=NULL) {
  newRulebase <- list(...)
  if (!missing(list))
    newRulebase <- c(newRulebase, list)
  
  class(newRulebase) <- c("rulebase", "list")
  newRulebase
}

##' @rdname rulesRulebases
##' @param x a rulebase object
##' @param ... passed on to print.rule
##' @export
print.rulebase <- function(x, ...) {
  rulebaseLength <- length(x)
  cat(rulebaseLength, if (rulebaseLength > 1) "rules:\n" else "rule:\n")
  rep <- sapply(x, function(i) c(deparse(i$ruleIf), deparse(i$ruleThen)))
  
  cat(paste(format(1:ncol(rep)), ": ",
            format(rep[1,]), " => ", format(rep[2,]),
            sep=""),
      sep="\n")
  invisible(x)
}


##' Convert a rule to a character string.
##'
##' @param x Rule
##' @param ... ignored
##' @return A string representing the rule.
##' @author Olaf Mersmann
as.character.rule <- function(x, ...) {
  lhs <- deparse(x$ruleIf)
  rhs <- deparse(x$ruleThen)
  sprintf("%s => %s", lhs, rhs)
}


##' Print a rule in human readable format.
##' 
##' @param x a rule
##' @param ... passed to print.default
##' @export
print.rule <- function(x, ...) {
  rep <- as.character(x, ...)
  cat(rep, "\n")
  invisible(rep)
}

##' Combine several rules / rulebases into a new rulebase.
##'
##' @param ... Rules or rulebases
##' @return The combined rulebase
##' @export
combineRulebase <- function(...) {
  structure(c(...),
            class="rulebase")
}

##' Repeatedly apply a rulebase to an input object
##'
##' Translate an input object by applying a rule base repeatedly, until the result does
##' not change anymore. \code{matcher} is tried on each rule in \code{rulebase} in order,
##' until a matching rule is found. \code{matcher} must return a binding object or \code{NA}.
##' When a rule matches, \code{action} is applied to the binding object and the rule's
##' consquence pattern, giving a result expression. After that, the rule base is applied
##' to the result expression again, if the result differs from \code{input}.
##' The \code{\link{simplify}} function gives a practical example for using
##' \code{applyRulebase} concerning the simplification of symbolic expressions.
##'
##' @param input The input object to translate.
##' @param rulebase The rulebase to use for translation.
##' @param matcher The function used to match the antecedens of each rule. Defaults to
##' \code{\link{patmatch}}.
##' @param action The action to apply when a rule matches.
##' @return The result of applying the \code{rulebase}.
##'
##' @export
applyRulebase <- function(input, rulebase, matcher = patmatch, action = substCall) {
  result <- input
  for (rule in rulebase) {
    bindings <- matcher(rule$ruleIf, input)
    if (!identical(NA, bindings)) {
      result <- action(bindings, rule$ruleThen)
      break
    }
  }
  result
}
