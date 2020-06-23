// Test simple var track

class SimpleVar {

  def nullable[T](x: T): T|Null = x

  locally {
    var x: String|Null = ???
    x = "" // x is assigned to a non-null value
    val l: Int = x.length  // ok, we know x is not null
  }

  locally {
    var x: String|Null = ???
    if (x != null) {
      val a: String = x
      x = ""
      val b: String = x
    }

    assert(x != null)
    val a: String = x
    x = nullable(x)
    val b: String = x // error: x might be null
  }

  locally {
    var x: String|Null = ???
    if (x != null) {
      val a: String = x
      x = null
      val b: String = x // error: x is null
    }
  }

  locally {
    var x: String|Null = ???
    if (x != null) {
      val a: String = x
      val b: String | String = a
      x = b
      val c: String = x // ok
    }
  }
}