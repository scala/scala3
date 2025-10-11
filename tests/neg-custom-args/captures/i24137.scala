//< using options -Ycc-verbose
import caps.{cap, Shared, SharedCapability}

open class A

class B(elem1: A^{cap.only[Shared]}, elem2: A^{cap.only[Shared]}):
  private var curElem: A^ = elem1 // problem is curElem contibutes cap to B().
  def next() =
    curElem = elem2

class Async extends caps.SharedCapability

def test(async: Async) =
  val a: A^{async} = A()
  val b = B(a, a)
  val _: B^{async} = b  // error, but could be OK if we have a way to mark
                        // curElem above as not contributing anything
  val b1: B^{async} = B(a, a) // error but could also be OK
