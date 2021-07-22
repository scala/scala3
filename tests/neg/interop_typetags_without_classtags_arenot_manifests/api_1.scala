package scala.reflect.api

trait TypeTags { self: Universe =>
  trait TypeTag[T]
}

abstract class Universe extends TypeTags
