object OpaqueTypes:
  opaque type OpaqueType[A] = List[A]
  object OpaqueType:
    def derived[A]: OpaqueType[A] = Nil

object FooModule:
  type Foo[A]
  object Foo:
    def derived[A]: Foo[A] = Nil.asInstanceOf[Foo[A]]

import FooModule.Foo
import OpaqueTypes.OpaqueType
case class Boom[A](value: A) derives OpaqueType, Foo // error // error

