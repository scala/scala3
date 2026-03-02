import scala.language.experimental.captureChecking
import scala.language.experimental.separationChecking

class Ref
class Box[T](val elem: T)

def h[T <: Ref^](x: Box[T]): Ref^ = x.elem // error

def hh[T <: Ref^](x: Box[T]): (Ref^, Int) = (x.elem, 1) // error (but msg is strange since it does not highlight the underlying box conflict)

def g[T <: Ref^](consume x: Box[T]): Ref^ = x.elem // error


def Test(rr: Ref^) =

  val r: Ref^ = Ref()
  val r2 = h(Box(r))
  println(r)

  def f[T <: Ref^{rr}](consume x: Box[T]): Ref^ = x.elem // error

