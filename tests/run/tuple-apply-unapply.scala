/** Test for uniform Tuple.apply
 *
 *  Tuple.apply: uniform construction for any arity with type preservation
 */
object Test extends App {

  // === APPLY TESTS ===
  println("=== Tuple.apply tests ===")

  // Arity 0
  val t0 = Tuple()
  assert(t0 == EmptyTuple)
  println(s"Tuple() = $t0")

  // Arity 1
  val t1 = Tuple(42)
  assert(t1 == Tuple1(42))
  println(s"Tuple(42) = $t1")

  // Arity 2
  val t2 = Tuple(1, "hello")
  assert(t2 == (1, "hello"))
  println(s"Tuple(1, hello) = $t2")

  // Arity 3
  val t3 = Tuple(1, "a", 3.14)
  assert(t3 == (1, "a", 3.14))
  println(s"Tuple(1, a, 3.14) = $t3")

  // Arity 22 (max specialized)
  val t22 = Tuple(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
  assert(t22.productArity == 22)
  println(s"Tuple(1..22) arity = ${t22.productArity}")

  // Arity 23 (TupleXXL)
  val t23 = Tuple(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23)
  assert(t23.productArity == 23)
  println(s"Tuple(1..23) arity = ${t23.productArity}")

  // === TYPE PRESERVATION TESTS ===
  println("=== Type preservation tests ===")

  // Verify types are preserved (these would fail to compile if types were wrong)
  val check0: EmptyTuple = t0
  val check1: Tuple1[Int] = t1
  val check2: (Int, String) = t2
  val check3: (Int, String, Double) = t3

  println("Type preservation: OK")

  // === UNAPPLY TESTS ===
  println("=== Tuple.unapply tests ===")

  // EmptyTuple
  EmptyTuple match
    case Tuple() => println("EmptyTuple: OK")

  // Tuple1 - typed extraction
  Tuple1(42) match
    case Tuple(x) =>
      val check: Int = x
      println(s"Tuple1: $x (typed Int)")

  // Tuple2 - typed extraction
  (1, "hello") match
    case Tuple(a, b) =>
      val checkA: Int = a
      val checkB: String = b
      println(s"Tuple2: $a, $b (typed)")

  // Tuple3 - typed extraction
  (1, "a", 3.14) match
    case Tuple(a, b, c) =>
      val checkA: Int = a
      val checkB: String = b
      val checkC: Double = c
      println(s"Tuple3: $a, $b, $c (typed)")

  // Abstract Tuple - unapplySeq fallback (elements are Any)
  println("=== Abstract Tuple (unapplySeq) ===")
  val abstractTuple: Tuple = (1, "hello", 3.14)
  abstractTuple match
    case Tuple(a, b, c) => println(s"Abstract 3-tuple: $a, $b, $c")
    case _ => assert(false, "Should have matched")

  // Arity mismatch - should not match
  abstractTuple match
    case Tuple(a, b) => assert(false, "Should not match wrong arity")
    case Tuple(a, b, c) => println("Correct arity matched")
    case _ => assert(false, "Should have matched")

  println("=== All tests passed ===")
}
