//> using options -source:future

// Under -source:future, a leading infix operator that follows an indented block
// and is less indented than that block applies to the enclosing control expression.

@main def Test =
  val a =
    if true then
      10
    else
      1
    + 100
  assert(a == 110)

  val b =
    if false then
      10
    else
      1
    + 100
  assert(b == 101)

  // usual precedence rules apply after the control expression
  val c =
    if true then
      10
    else
      1
    + 2 * 100
  assert(c == 210)

  // `else if` chains: the operator applies to the outermost conditional
  val d =
    if false then
      1
    else if false then
      2
    else
      3
    + 100
  assert(d == 103)

  // the operator lines up with the inner conditional, which has its own indentation region
  val e =
    if false then
      1
    else
      if false then
        2
      else
        3
      + 100
  assert(e == 103)

  val f =
    for x <- List(1, 2) yield
      x * 10
    ++ List(3)
  assert(f == List(10, 20, 3))

  val g =
    try 1
    catch case _: Exception =>
      2
    + 100
  assert(g == 101)

  val h =
    try "a"
    finally
      "b"
    + "c"
  assert(h == "ac")

  // an indented operator still belongs to the branch
  val i =
    if true then
      10
    else
      1
      + 100
  assert(i == 10)

  // single-line branches are not affected
  val j =
    if true then 10 else 1
    + 100
  assert(j == 10)

  // inline if
  inline def k(inline x: Boolean) =
    inline if x then
      10
    else
      1
    + 100
  assert(k(true) == 110)
