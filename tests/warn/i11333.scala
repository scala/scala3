

class C:
  val f1: Float  = 123456789            // warn
  val d1: Double = 1234567890123456789L // warn
  val f2: Float  = 123456789L           // warn

  inline val i1 = 123456789
  inline val l1 = 1234567890123456789L
  inline val l2 = 123456789L

  val f1_b: Float  = i1 // warn
  val d1_b: Double = l1 // warn
  val f2_b: Float  = l2 // warn