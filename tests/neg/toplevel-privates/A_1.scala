package p

private val x = 10

val y = x

private[p] val xx = 10

val yy = xx

private class C {
  val z = x
}

