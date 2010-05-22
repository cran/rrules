## simplify.r
##   - Rule based simplification of R expressions and functions
##
## Rrules - a simple rule based translator for R
## 2010 Oliver Flasch (oliver.flasch@fh-koeln.de)
## with contributions of Thomas Bartz-Beielstein, Olaf Mersmann and Joerg Stork
## released under the GPL v2
##

##' Simplify a symbolic expression
##'
##' Tries to transform a symbolic expression to an equivalent but simpler form.
##' \code{simplify} has methods for simplifying functions and calls.
##'
##' @param x The expression to simplify.
##' @param algorithm The algorithm to use for simplifying \code{x}. Defaults to
##' \code{\link{rulebasedSimplify}}.
##' @param ... Additional arguments to \code{algorithm}.
##' @return The simplified expression.
##' @export
simplify <- function(x, algorithm = rulebasedSimplify, ...) UseMethod("simplify")

##' @rdname simplify
##' @method simplify function
##' @S3method simplify "function"
simplify.function <- function(x, algorithm = rulebasedSimplify, ...) {
  simplifiedFunction <- x
  body(simplifiedFunction) <- simplify.call(body(x), algorithm, ...)
  simplifiedFunction
}

##' @rdname simplify
##' @method simplify call
##' @S3method simplify call
simplify.call <- function(x, algorithm = rulebasedSimplify, ...) {
  if (is.call(x)) {
    simplifiedX <- algorithm(as.call(Recall(as.list(x), algorithm)), ...)
    if (!identical(x, simplifiedX)) # simplify as far as possible
      Recall(simplifiedX, algorithm, ...)
    else
      simplifiedX
  } else if (is.composite(x)) {
    c(Recall(first(x), algorithm), Recall(rest(x), algorithm))
  } else if (is.atom(x) && is.list(x)) { # R does not distinguish between atoms and 1-element lists
    Recall(first(x))
  } else {
    x
  }
}

##' Simplifies an expression using a rule based simplification algorithm.
##'
##' \code{arithmeticSimplificationRules} contains rules for simplifying arithmetic
##' expressions, \code{logarithmSimplificationRules} contains rules for simplifying
##' expressions with logarithms and exponents, \code{trigonometricSimplificationRules}
##' contains rules for simplifying expressions with sines and cosines.
##' \code{basicSimplificationRules} contains all of the above rules.
##'
##' @param x The expression to simplify.
##' @param rulebase The rulebase to use for simplification. Defaults to
##' \code{\link{basicSimplificationRules}}.
##' @param evaluate Whether to evaluate constant subexpressions during simplification.
##' @return The simplified expresssion.
##'
##' @rdname ruleBasedExpressionSimplification
##' @export
rulebasedSimplify <- function(x, rulebase = basicSimplificationRules, evaluate = TRUE) {
  if (evaluate && is.evaluable(x)) x <- eval(x)
  applyRulebase(x, rulebase)
}

##' @rdname ruleBasedExpressionSimplification
##' @export
arithmeticSimplificationRules <-
  rulebase(rule( v.X + 0           , v.X     ),
           rule( 0 + v.X           , v.X     ),
           rule( v.X + v.X         , 2 * v.X ),
           rule( v.X - 0           , v.X     ),
           rule( 0 - v.X           , -v.X    ),
           rule( v.X - v.X         , 0       ),
           rule( +-v.X             , -v.X    ), ## Mixing unary and binary -/+
           rule( -+v.X             , -v.X    ), ## Mixing unary and binary -/+
           rule( --v.X             , v.X     ),
           rule( v.X * 1           , v.X     ),
           rule( 1 * v.X           , v.X     ),
           rule( v.X * 0           , 0       ),
           rule( 0 * v.X           , 0       ),
           rule( v.X * v.X         , v.X ^ 2 ),
           rule( v.X / 0           , Inf     ),
           rule( 0 / v.X           , 0       ),
           rule( v.X / 1           , v.X     ),
           rule( v.X / v.X         , 1       ),
          #rule( 0 ^ 0             , NaN     ), # R thinks 0 ^ 0 is 1
           rule( v.X ^ 0           , 1       ),
           rule( 0 ^ v.X           , 0       ),
           rule( 1 ^ v.X           , 1       ),
           rule( v.X ^ 1           , v.X     ),
           rule( v.X * (v.Y / v.X) , v.Y     ),
           rule( (v.Y / v.X) * v.X , v.Y     ),
           rule( (v.Y * v.X) / v.X , v.Y     ),
           rule( (v.X * v.Y) / v.X , v.Y     ),
           rule( v.X + -v.X        , 0       ),
           rule( (-v.X) + v.X      , 0       ),
           rule( v.X + v.Y - v.X   , v.Y     ),
          #rule( v.A * v.X + v.B * v.X, (v.A + v.B) * v.X ), # Beispiel Olaf
           rule( (v.X)             , v.X     )) # remove single parens

##' @rdname ruleBasedExpressionSimplification
##' @export
logarithmSimplificationRules <-
  rulebase(rule( (v.X ^ v.Y) * (v.X ^ v.Z) , v.X ^ (v.Y + v.Z) ),
           rule( (v.X ^ v.Y) / (v.X ^ v.Z) , v.X ^ (v.Y - v.Z) ),
           rule( log(v.X) + log(v.Y)       , log(v.X * v.Y)    ),
           rule( log(v.X) - log(v.Y)       , log(v.X / v.Y)    ))

##' @rdname ruleBasedExpressionSimplification
##' @export
trigonometricSimplificationRules <-
  rulebase(rule( sin(v.X) ^ 2 + cos(v.X) ^ 2, 1 ))

##' @rdname ruleBasedExpressionSimplification
##' @export
basicSimplificationRules <-
  combineRulebase(arithmeticSimplificationRules,
                  logarithmSimplificationRules,
                  trigonometricSimplificationRules)

