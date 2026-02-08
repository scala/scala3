sealed trait A:
  type Blah <: reflect.Enum

case object D extends A:
  override abstract class Blah extends reflect.Enum // error
