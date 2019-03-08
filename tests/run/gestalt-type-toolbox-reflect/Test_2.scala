
object Test {
  import TypeToolbox._

  type Age = Int

  def main(args: Array[String]): Unit = {

    def test(name: String)(tests: => Unit): Unit = {
      println("Testing " + name)
      tests
    }

    test("=:=") {
      assert(=:=[Nil.type, Nil.type])
      assert(=:=[Int, Int])
      assert(! =:=[Int, String])
      assert(=:=[Int, Age])
    }

    test("<:<") {
      assert(<:<[Int, Int])
      assert(<:<[Age, Int])
      assert(<:<[None.type, Option[Int]])
      assert(<:<[Nil.type, List[Int]])
      assert(! <:<[Int, String])

      val a = 5
      assert(<:<[3, Int])
      assert(<:<[a.type, Int])
    }

    // TODO
    // test("typeRef") {
    //   assert(typeRef[String]("java.lang.String"))
    //   assert(typeRef[String]("scala.String"))
    //   assert(typeRef[Int]("scala.Int"))
    //   assert(typeRef[Boolean]("scala.Boolean"))
    // }

    // TODO
    // test("termRef") {
    //   assert(termRef[None.type]("scala.None"))
    //   assert(termRef[Nil.type]("scala.Nil"))
    // }

    test("isCaseClass") {
      case class Student(name: String, age: Int)
      class Teacher(name: String, age: Int)
      trait Staff
      assert(isCaseClass[Student])
      assert(!isCaseClass[Teacher])
      assert(!isCaseClass[Staff])

    }

    test("caseFields") {
      case class Student(name: String, age: Int)
      case class Teacher(name: String, age: Int) {
        val school: String = "EPFL"
        var salary: Int = 20000
      }

      assert(caseFields[Student] == List("name", "age"))
      assert(caseFields[Teacher] == List("name", "age"))
    }

    // TODO
    // test("asSeenFrom") {
    //   case class M[T](x: T)
    //   val a = new M(4)

    //   assert(fieldType[a.type, Int]("x"))


    //   trait Base {
    //     val x: InBase
    //     trait InBase
    //   }

    //   class Child extends Base {
    //     val x: Inner = new Inner
    //     class Inner extends InBase
    //   }

    //   val m = new Child
    //   assert(fieldType[m.type, m.Inner]("x"))

    //   trait Box {
    //     type T
    //     val x: T
    //   }
    //   class InBox extends Box {
    //     type T = Int
    //     val x = 3
    //   }
    //   val box = new InBox
    //   assert(fieldType[box.type, Int]("x"))
    // }

    test("fields") {
      trait Base {
        val x = 3

        def f = 4
      }

      class Derived extends Base {
        val y = 3

        def g = 3
      }

      assert(fieldIn[Base]("x") == "x")
      assert(fieldIn[Derived]("x") == "")
      assert(fieldIn[Derived]("y") == "y")
      assert(fieldsIn[Base] == List("x"))
      assert(fieldsIn[Derived] == List("y"))
    }

    test("methods") {
      trait Base {
        val x = 3

        def f = 4
      }

      class Derived extends Base {
        val y = 3

        def g = 3
      }

      assert(method[Base]("f") == List("f"))
      assert(method[Base]("x") == Nil)
      assert(methodIn[Base]("f") == List("f"))
      assert(methodIn[Base]("x") == Nil)
      assert(methodsIn[Base] == List("f"))

      assert(method[Derived]("f") == List("f"))
      assert(method[Derived]("g") == List("g"))
      assert(method[Derived]("y") == Nil)
      assert(methodIn[Derived]("f") == Nil)
      assert(methodIn[Derived]("g") == List("g"))
      assert(methodIn[Derived]("x") == Nil)
      assert(methodsIn[Derived] == List("g"))

      class Overloading {
        def f(a: Int): Int = ???
        def f(a: String): Int = ???
      }

      assert(methodsIn[Overloading] == List("f", "f"))
      assert(methodIn[Overloading]("f") == List("f", "f"))
    }

    test("typeTag") {
      assert(typeTag(3) == "scala.Int")
      assert(typeTag(Some(4)) == "scala.Some[scala.Int]", typeTag(Some(4)))
    }

    test("companion") {
      class A
      object A
      class B
      object C
      assert(companion[A, A.type])
      assert(companionName[A] == "Test$._$A")
      assert(companionName[A.type] == "Test$._$A")
      assert(companionName[B] == "", companionName[B])
      assert(companionName[C.type] == "", companionName[C.type])
    }
  }
}
