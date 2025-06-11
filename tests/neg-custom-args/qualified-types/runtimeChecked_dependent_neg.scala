def foo(x: Int, y: {v: Int with v > x}): y.type = y

def getInt(): Int =
  println("getInt called")
  42

@main def Test =
  val res = foo(getInt(), 2.runtimeChecked) // error
