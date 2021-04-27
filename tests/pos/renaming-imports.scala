import java as j
import collection as c

val x: j.io.IOException = ???

import c.mutable as mut
import mut.ArrayBuffer as Buf

val y = Buf(1, 2, 3)

object O:
  type OString = String
  def foo22(x: Int) = x

class C:
  import O.*
  import foo22 as foo
  import OString as OS
  import scala.collection.Iterable
  println(foo(22))
  val s: OS = ""

def test =
  import C as CC
  println(C().s)


