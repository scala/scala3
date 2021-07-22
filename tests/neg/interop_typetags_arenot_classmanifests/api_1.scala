package scala.reflect.api

trait TypeTags { self: Universe =>
  trait WeakTypeTag[T]
  trait TypeTag[T] extends WeakTypeTag[T]
}

abstract class Universe extends TypeTags
