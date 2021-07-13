val `Array[Nothing]` = manifest[Array[Nothing]] // error
val `Array[Null]` = manifest[Array[Null]] // error
val m_Nothing = manifest[Nothing] // error
val m_Null = manifest[Null] // error

val `Array[? <: Nothing]` = manifest[Array[? <: Nothing]] // error
val `Array[? <: Null]` = manifest[Array[? <: Null]] // error

val `Int @unchecked` = manifest[Int @unchecked] // error

val `0 | 1` = manifest[0 | 1] // error

class Box[T] {
  val m = manifest[T] // error
}

object Foo {
  type F[T] <: T
  manifest[Array[F[Int]]] // error
}

object opaques {
  opaque type OpaqueList[+A] = List[A]
}
import opaques.*

val `OpaqueList[Int]` = manifest[OpaqueList[Int]] // error (opaque types are not supported)
