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
  if (x != null) {
    val a: String = x
    x = null
    val b: String = x // error: x is null
  }
}

def h = {
  var x: String|Null = ???
  if (x != null) {
    val a: String = x
    val b: String | String = a
    x = b
    val _: String = x // ok
  }
}