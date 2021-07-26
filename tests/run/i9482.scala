import scala.reflect.{OptManifest, ClassTag}

object Ref {

  object Sentinel

  def makeWithArr[A: OptManifest]: String = optManifest[A] match {
    case m: ClassTag[_] => m.newArray(0).asInstanceOf[AnyRef] match {
      // these can be reordered, so long as Unit comes before AnyRef
      case _: Array[Boolean] => "bool"
      case _: Array[Byte]    => "byte"
      case _: Array[Short]   => "short"
      case _: Array[Char]    => "char"
      case _: Array[Int]     => "int"
      case _: Array[Float]   => "float"
      case _: Array[Long]    => "long"
      case _: Array[Double]  => "double"
      case _: Array[Unit]    => "unit"
      case a: Array[AnyRef]  => a.getClass.getComponentType.getName
    }
    case _ => "<?>"
  }

  def make[A: OptManifest]: String = optManifest[A] match {
    case m: ClassTag[a] => m match {
      case ClassTag.Boolean => "bool"
      case ClassTag.Byte    => "byte"
      case ClassTag.Short   => "short"
      case ClassTag.Char    => "char"
      case ClassTag.Int     => "int"
      case ClassTag.Float   => "float"
      case ClassTag.Long    => "long"
      case ClassTag.Double  => "double"
      case ClassTag.Unit    => "unit"
      case ClassTag.Any     => "any"
      case ClassTag.AnyVal  => "anyval"
      case ClassTag.Object  => "anyref"
      case _                => m.runtimeClass.getName
    }
    case NoManifest => "<?>"
  }

}

import Ref.*

def baz[A] = Ref.makeWithArr[A]
def qux[A] = Ref.make[A]

@main def Test = {

  assert(Ref.makeWithArr[Boolean] == "bool")
  assert(Ref.makeWithArr[Byte]    == "byte")
  assert(Ref.makeWithArr[Short]   == "short")
  assert(Ref.makeWithArr[Char]    == "char")
  assert(Ref.makeWithArr[Int]     == "int")
  assert(Ref.makeWithArr[Float]   == "float")
  assert(Ref.makeWithArr[Long]    == "long")
  assert(Ref.makeWithArr[Double]  == "double")
  assert(Ref.makeWithArr[Unit]    == "unit")
  assert(Ref.makeWithArr["abc"]   == "java.lang.String")
  assert(Ref.makeWithArr[Null]    == "<?>")
  assert(Ref.makeWithArr[Nothing] == "<?>")
  assert(baz[Int]                 == "<?>")

  assert(Ref.make[Boolean] == "bool")
  assert(Ref.make[Byte]    == "byte")
  assert(Ref.make[Short]   == "short")
  assert(Ref.make[Char]    == "char")
  assert(Ref.make[Int]     == "int")
  assert(Ref.make[Float]   == "float")
  assert(Ref.make[Long]    == "long")
  assert(Ref.make[Double]  == "double")
  assert(Ref.make[Unit]    == "unit")
  assert(Ref.make[Any]     == "any")
  assert(Ref.make[AnyVal]  == "anyval")
  assert(Ref.make[AnyRef]  == "anyref")
  assert(Ref.make["abc"]   == "java.lang.String")
  assert(Ref.make[Null]    == "<?>")
  assert(Ref.make[Nothing] == "<?>")
  assert(qux[Int]          == "<?>")

}
