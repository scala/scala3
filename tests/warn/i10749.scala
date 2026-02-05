//> using options -Wshadow:pattern-variable-shadow

object Test:

  // Test 1: Pattern shadows local val
  def test1 =
    val x = 1
    Some(2) match
      case Some(x) => x  // warn: pattern variable shadows outer x
      case None => 0

  // Test 2: Pattern shadows parameter
  def test2(y: Int) = y match
    case y => y  // warn: pattern variable shadows parameter y

  // Test 3: Wildcard is OK
  def test3 =
    val z = 1
    Some(2) match
      case Some(_) => 0  // ok: wildcard doesn't create binding
      case None => z

  // Test 4: No shadowing
  def test4 =
    Some(2) match
      case Some(fresh) => fresh  // ok: no shadowing
      case None => 0

  // Test 5: Nested match shadows outer pattern variable (not just outer val)
  def test5 =
    val outer = 1
    Some(Some(2)) match
      case Some(Some(outer)) => outer  // warn: shadows outer val
      case _ => 0

  // Test 6: Multiple patterns in a tuple
  def test6 =
    val a = 1
    val b = 2
    (Some(1), Some(2)) match
      case (Some(a), Some(b)) => a + b  // warn // warn
      case _ => 0

  // Test 7: Pattern in for-comprehension
  def test7 =
    val x = 1
    for
      Some(x) <- List(Some(1), None)  // warn: shadows x
    yield x

  // Test 8: Stable identifier - should NOT warn (different semantics)
  def test8 =
    val y = 1
    Some(1) match
      case Some(`y`) => 1  // ok: backticks mean stable identifier match, not new binding
      case _ => 0
