object Foo {

  object opaques {
    opaque type Inner = String
    val i: Inner = "i"
  }

  val singleton: opaques.Inner = opaques.i

  val om_Inner     = optManifest[opaques.Inner] // NoManifest
  val om_singleton = optManifest[singleton.type] // NoManifest
  val ct_Inner     = reflect.classTag[opaques.Inner]
  val ct_singleton = reflect.classTag[singleton.type]
}

val `List[Nothing]` = manifest[List[Nothing]]
val `List[Array[Nothing]]` = manifest[List[Array[Nothing]]] // ok when Nothing is not the argument of top-level array

val `Array[Array[List[Int]]]` = manifest[Array[Array[List[Int]]]]

trait Mixin[T <: Mixin[T]] { type Self = T }
class Baz extends Mixin[Baz] { val m = manifest[Self] }
