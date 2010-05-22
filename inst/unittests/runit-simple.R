##
## runit-simple.R - simple unit tests for the simplify() function
##

## Generate a test function which tests that e == simplify(e)
invariance_test <- function(e) {
  E <- substitute(e)
  ## Fancy way to define the function so that the true expressions are
  ## visible in the RUnit log instead of T and F.
  f <- function() { }
  body(f) <- substitute(checkIdentical(quote(E), simplify(quote(E))), list(E=E))
  rm(E) ## Clean up environment
  f
}

ts <- defineTestSuite(name="Oink", dirs="skel/inst/unittests")

simplify_to <- function(from, to) {
  F <- substitute(from)
  T <- substitute(to)
  ## Fancy way to define the function so that the true expressions are
  ## visible in the RUnit log instead of T and F.
  f <- function() { }
  body(f) <- substitute(checkIdentical(quote(T), simplify(quote(F))), list(F=F, T=T))
  rm(F, T) ## Clean up environment
  f
}

## Basic invariance properties:
test.inv             <- invariance_test(as.symbol(a))
test.inv_add         <- invariance_test(a + b)
test.inv_multiply    <- invariance_test(a * b)

## Negation rules:
test.neg_1           <- simplify_to(+-a, -a)
test.neg_2           <- simplify_to(-+a, -a)
test.neg_3           <- simplify_to(+-+a, -a)

test.double_neg_1    <- simplify_to(--a, a)
test.double_neg_2    <- simplify_to(-+-a, a)

## Addition rules:
test.add_null_1      <- simplify_to(a + 0, a)
test.add_null_2      <- simplify_to(0 + a, a)
test.add_null_3      <- simplify_to(a + 0 + b, a + b)
test.add_null_4      <- simplify_to(0 + a + 0, a)

test.add_self_1      <- simplify_to(a + a, 2*a)

test.add_neg_self_1  <- simplify_to(a + -a, 0)

#test.add_neg_self_2  <- simplify_to((-a) + a, 0) ## FAILS because  rule((-v.X) + v.X, 0) does not match:
#test.add_neg_self_3  <- simplify_to(-a + a, 0)   ## FAILS because  rule((-v.X) + v.X, 0) does not match:

## Multiplication rules:
test.multiply_one_1  <- simplify_to(a * 1, a)
test.multiply_one_2  <- simplify_to(1 * a, a)
test.multiply_one_3  <- simplify_to(a * 1 * b, a * b)
test.multiply_one_4  <- simplify_to(1 * a * 1, a)

test.multiply_null_1 <- simplify_to(a * 0, 0)
test.multiply_null_2 <- simplify_to(0 * a, 0)
test.multiply_null_3 <- simplify_to(a * 0 * b, 0)
test.multiply_null_4 <- simplify_to(0 * a * 0, 0)

test.multiply_self_1 <- simplify_to(a * a, a^2)

## Division rules:
test.divide_one_1    <- simplify_to(a/1, a)

test.divide_null_1   <- simplify_to(a/0, Inf)

test.divide_self_1   <- simplify_to(a/a, 1)
test.divide_self_2   <- simplify_to((a+b)/(a+b), 1)

## Multiply-add rules:
test.multiply_add_1  <- simplify_to(1 * a + 0, a)
test.multiply_add_2  <- simplify_to(a * 1 + 0, a)
test.multiply_add_3  <- simplify_to(0 + 1 * a, a)
test.multiply_add_4  <- simplify_to(0 + a * 1, a)

## Power rules:
test.power_null_1    <- simplify_to(a^0, 1)
test.power_null_2    <- simplify_to(0^a, 0)

test.power_one_1     <- simplify_to(1^a, 1)
test.power_one_2     <- simplify_to(a^1, a)
