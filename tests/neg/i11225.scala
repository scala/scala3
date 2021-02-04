import compiletime.uninitialized

class Test:

  val x1: Int = uninitialized // error
  var x2: Int = if ??? then uninitialized else uninitialized // error // error
  var x3: Int = if true then uninitialized else 1 // ok
  var x4: Int = if false then uninitialized else 1 // ok
  var x5: () => Int = () => uninitialized // error
  var x6: Int = { uninitialized } // error

  def f(x: Boolean) =
    var cached: Int = uninitialized   // error
    cached = if x then 1 else uninitialized  // error

  var c: Int =
    uninitialized // error
    uninitialized // error
    2

  var d: Int =
    println("pseudo init")
    uninitialized // error

  transparent inline def uni = uninitialized

  inline def g(inline x: Int): Unit = ()
  def f2 = g(uninitialized) // this one is ok since `uninitialized` is inlined away

  var x7: Int = uni  // error
