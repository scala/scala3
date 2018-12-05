
// Flow-sensitive type inference
class Foo {

  class Bar {
    val s: String = ???
  }

  val b: Bar|Null = ???
  if (b != null) {
    val s = b.s   // ok: type of `b` inferred as `Bar`
    val s2: Bar = b
  } else {
    val s = b.s   // error: `b` is `Bar|Null`
  }
  val s = b.s     // error: `b` is `Bar|Null`


  var b2: Bar|Null = ???
  if (b2 != null) {
    val s = b.s // error: type of `b2` isn't refined because `b2` is not stable
  }

  class Bar2 {
    val x: Bar2|Null = ???
  }

  val bar2: Bar2|Null = ???
  if (bar2 != null) {
    if (bar2.x != null) {
      if (bar2.x.x != null) {
        if (bar2.x.x.x != null) {
          val b2: Bar2 = bar2.x.x.x
        }
        val b2: Bar2 = bar2.x.x
        val b2err: Bar2 = bar2.x.x.x // error: expected Bar2 but got Bar2|Null
      }
      val b2: Bar2 = bar2.x
    }
    val b2: Bar2 = bar2
  }

}
