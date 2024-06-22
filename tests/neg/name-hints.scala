object O:
  val abcde: Int = 0
  val xy: Int = 1

object Test:
  val x1 = Int.maxvalue  // error
  val x2 = Int.MxValue   // error
  val x3 = Int.MaxxValue // error

  val d1 = O.abcd        // error
  val d2 = O.abc         // error
  val d3 = O.ab          // error, no hint since distance = 3 > 2 = length
  val s1 = O.Abcde       // error
  val s3 = O.AbCde       // error
  val s3 = O.AbcdE       // error
  val s3 = O.AbCDE       // error, no hint
  val a1 = O.abcde0      // error
  val a2 = O.abcde00     // error
  val a3 = O.abcde000    // error
  val a4 = O.abcde0000   // error, no hint

  val y1 = O.x           // error, no hint
  val y2 = O.xY          // error
  val y3 = O.xyz         // error
  val y2 = O.XY          // error, no hint

