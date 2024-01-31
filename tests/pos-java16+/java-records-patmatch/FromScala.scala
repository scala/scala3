object C:

  def useR0: Unit =
    val r = R0()

    // unapply in valdef
    val R0() = r

    // unapply in patmatch
    r match {
      case R0() =>
    }


  def useR1: Int =
    val r = R1(1, "foo")

    // unapply in valdef
    val R1(i, _) = r
    val a: Int = i

    // unapply in patmatch
    r match {
      case R1(i, _) => i
    }
