##
## runit-regression.R - unit tests to avoid regressions
##

test.override_rulebase_1 <- function() {
  rb <- rulebase()
  checkIdentical(quote(a*a),
                 simplify(quote(a*a), rulebase=rb))
}

test.override_rulebase_2 <- function() {
  rb <- rulebase(rule(log(a*b), log(a) + log(b)))
  checkIdentical(quote(log(a) + log(b)),
                 simplify(quote(log(a*b)), rulebase=rb))
}

test.simplify_function <- function() {
  f1 <- function() { x * x + 0 }
  f2 <- function() { x^2 }
    
  checkIdentical(f2, simplify(f1))
}
