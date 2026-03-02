//> using options -Wunused:imports

package p:

  trait Base
  class Class extends Base

  abstract class Entity[T: GetType]

  class Thing extends Entity[Class]

  trait GetType[T]

  object GetType {
    //implicit object GetTypeClass extends GetType[Class]
    implicit val GetTypeClass: GetType[Class] = new GetType[Class] {}
  }
  object Main {
    def main(args: Array[String]): Unit = {
      import GetType.*
      val e = GetTypeClass
    }
  }

package q:

  class C:
    def f =
      import p.*
      GetType.GetTypeClass
    def g =
      import p.GetType.*
      GetTypeClass
  class D extends p.Entity[p.Class]
