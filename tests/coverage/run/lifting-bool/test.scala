
def notCalled() = ???

def f(x: Boolean, y: Boolean): Boolean = x

@main
def Test: Unit =
  val a = true || notCalled()            // true
  val b = false && notCalled()           // false
  val c = (true || false) || notCalled() // true
  val d = true && (false && notCalled()) // false
  val e = (true && false) && notCalled() // false
  println(s"$a $b $c $d $e")

  var x = f(true, false)
  println(x) // true

  x = f(true || notCalled(), false && notCalled())
  println(x) // true
