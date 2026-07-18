//< using options -Ycc-verbose
import caps.{any, Shared, SharedCapability}
import caps.unsafe.untrackedCaptures
open class A

class B(elem1: A^{any.only[Shared]}, elem2: A^{any.only[Shared]}):
  @untrackedCaptures private var curElem: A^ = elem1 // problem is curElem contibutes any to B().
  def next() =
    curElem = elem2

class Async extends caps.SharedCapability

def test(async: Async) =
  val a: A^{async} = A()
  val b = B(a, a)
  val _: B^{async} = b  // was error, but could be OK if we have a way to mark
                        // curElem above as not contributing anything
  val b1: B^{async} = B(a, a) // was error but could also be OK
