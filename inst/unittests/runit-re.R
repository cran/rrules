##
## runit-re.R - direct tests of the rule engine
##

## Check one rule against a given input and expected output
one_rule_check <- function(rule, from, to) {
  rb <- rulebase(rule)
  ## Fancy way to define the function so that the true expressions are
  ## visible in the RUnit log instead of T and F.
  f <- function() { }
  body(f) <- substitute(checkIdentical(quote(T), applyRulebase(quote(F), rb)),
                        list(F=substitute(from), T=substitute(to)))
  f
}

## See test.add_neg_self_[23] for motivation:
test.rb_add_neg_self_1 <- one_rule_check(rule((-v.X) + v.X, 0), (-a) + a, 0)
