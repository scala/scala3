import language.implicitConversions
given Conversion[Int, String] = _.toString

object a:
  val s: String = 1   // OK

  object b:
    import language.implicitConversions as _
    val s: String = 2   // error

    object c:
      import language.implicitConversions
      val s: String = 3   // OK again
