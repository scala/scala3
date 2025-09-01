import language.experimental.captureChecking
import caps.*

trait Ctx[T]

def test =
  val x: Any^ = ???
  val y: Any^ = ???
  object O:
    val z: Any^ = ???
  trait CaptureSet:
    type A^ >: {y} <: {x}
    type B^ = {x}
    type C^ <: {x}
    type D^ : Ctx // error
    type E^ <: {C}
    type F^ <: {C}
    type G^ <: {x, y}
    type H^ >: {x} <: {x,y} : Ctx // error
    type I^ = {y, G, H}
    type J = {O.z}
    type K^ = {x, O.z}
    type L^ <: {x, y, O.z}
    type M^ >: {x, y, O.z} <: {C}
    type N^ >: {x} <: {x}
    type O^ >: {O.z} <: {O.z}
    type P^ >: {B,D} // error
    type Q^ >: {E} <: Int // error
    type R^ >: {E} <: Int // error
    type S^ >: Nothing <: Int // error
    type T >: Nothing <: Int