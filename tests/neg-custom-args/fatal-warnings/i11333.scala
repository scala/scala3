class C:
  val f1: Float  = 123456789            // error
  val d1: Double = 1234567890123456789L // error
  val f2: Float  = 123456789L           // error

  inline val i1 = 123456789
  inline val l1 = 1234567890123456789L
  inline val l2 = 123456789L

  val f1_b: Float  = i1 // error
  val d1_b: Double = l1 // error
  val f2_b: Float  = l2 // error
