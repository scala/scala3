import scala.compiletime.erasedValue

class Box[T]

trait T:
  type L

class A extends T:
  type L <: Int

class B extends T:
  type L >: String

inline def f() =
  inline erasedValue[A & B] match
    case x: (A & B) =>
      val y: String = "String"
      val z: x.L = y
      z: Int

@main def Test =
  println(f().abs) // error
