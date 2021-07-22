package scala.reflect.api

import scala.reflect.{Manifest, ClassTag}

trait TypeTags { self: Universe =>
  trait TypeTag[T]
}

trait Internals { self: Universe =>

  object internal {
    def typeTagToManifest[T: ClassTag](mirror: Any, tag: Universe#TypeTag[T]): Manifest[T] = ???
  }

}

abstract class Universe extends TypeTags
                           with Internals

abstract class JavaUniverse extends Universe {
  def runtimeMirror(cl: ClassLoader): Any = ???
}
