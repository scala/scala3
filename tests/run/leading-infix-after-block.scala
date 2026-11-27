//> using options -source:future

// Under -source:future, a leading infix operator that directly follows a block,
// i.e. an <outdent> or a closing brace, applies to the control expression of which
// the block is a branch or body.

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

  // the same with a single-line `then` branch in the inner conditional
  val d2 =
    if true then
      1
    else if true then 2
    else
      3
    + 100
  assert(d2 == 101)

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

  // blocks in braces are treated the same way
  val l =
    if true then {
      1
    } else {
      2
    }
    + 100
  assert(l == 101)

  val m =
    if false then {
      1
    }
    else {
      2
    }
    + 100
  assert(m == 102)

  // a block in braces that is not the first operand is not affected
  val n =
    1 + {
      2
    }
    + 100
  assert(n == 103)

  // inline if
  inline def k(inline x: Boolean) =
    inline if x then
      10
    else
      1
    + 100
  assert(k(true) == 110)

  // blocks that are not a branch or body of a control expression are not affected:
  // the operator applies to the block, as with a hanging operator
  val o =
    val x =
      1
    + 100
    x
  assert(o == 101)

  val p =
    val x = {
      1
    }
    + 100
    x
  assert(p == 101)

  val q = (x: Int) =>
    x
  + 100
  assert(q(1) == 101)

  val r =
    try 1
    catch case _: Exception =>
      2
    + 100
  assert(r == 1)
