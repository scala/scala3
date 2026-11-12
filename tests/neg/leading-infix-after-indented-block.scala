//> using options -source:future

// Under -source:future, a leading infix operator after an indented block
// is rejected if there is no enclosing control expression.

def f(a: Boolean, b: Boolean) =
  val x =
    !a
  || b // error
  x

def g =
  val h = (x: Int) =>
    x
  + 100 // error
  h
