class S {
  given Conversion[String, Array[String]] = _ => ???

  def f = {
    val s: String | Null = ???

    val x: String = s // error
    val xl = s.length // error
    val xs: Array[String | Null] | Null = s // error

    {
      import scala.language.unsafeNulls
      // ensure the previous search cache is not used here
      val y: String = s
      val yl = s.length
      val ys: Array[String | Null] | Null = s

      {
        // disable unsafeNulls here
        import scala.language.unsafeNulls as _
        val z: String = s // error
        val zl = s.length // error
        val zs: Array[String | Null] | Null = s // error
      }
    }

    val z: String = s // error
    val zl = s.length // error
    val zs: Array[String | Null] | Null = s // error
  }
}