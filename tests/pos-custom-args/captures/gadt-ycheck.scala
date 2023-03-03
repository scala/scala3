package test

import reflect.ClassTag
import language.experimental.pureFunctions

object Settings:
  val OptionTag: ClassTag[Option[?]] = ClassTag(classOf[Option[?]])

  class Setting[T: ClassTag](propertyClass: Option[Class[?]]):
    def tryToSet() =
      def update(value: Any): String = ???
      implicitly[ClassTag[T]] match
        case OptionTag =>
          update(Some(propertyClass.get.getConstructor().newInstance()))
