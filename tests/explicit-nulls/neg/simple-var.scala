// Test simple var track

def nullable[T](x: T): T|Null = x

def f = {
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

def g = {
  var x: String|Null = ???
  lazy val y = {
    if (x != null) {
      x = null
    }

  }
  if (x != null) {
    val a: String = x // error: x exists in closure, no longer tackable
  }
}

def h = {
  var x: String|Null = "???"
  lazy val y = {
    if (x != null) {
      val a: String = x // error: x exists in closure, no longer tackable
    }
    x
  }
}

def i = {
  var x: String|Null = "???"
  def y = {
    if (x != null) {
      val a: String = x // error: x exists in closure, no longer tackable
    }
    x
  }
}