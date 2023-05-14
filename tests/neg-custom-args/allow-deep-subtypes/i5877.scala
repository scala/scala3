object Main {
    def main(a: Array[String]): Unit = {
      println("you may not run `testHasThisType` - just check that it compiles")
      // comment lines after "// this line of code makes" comments to make it compilable again
      testHasThisType()
      testHasThisType2()
    }

    // ---- ---- ---- ----

    trait HasThisType[PThis <: HasThisType[_ <: PThis]] {
      this: PThis =>
      type This = PThis

      // inline // uncommenting `inline` cause problem in scastie dotty version, but is fixed in dotty `master`
      def self(): This with this.type = this
    }

    // ---- ---- ---- ----

    def testHasThisType(): Unit = {
      def testSelf[PThis <: HasThisType[_ <: PThis]](that: HasThisType[PThis]): Unit = {
        val thatSelf = that.self()
        // that.self().type <: that.This
        assert(implicitly[thatSelf.type <:< that.This] != null)
      }
      val that: HasThisType[_] = Foo() // null.asInstanceOf
      testSelf(that) // error
    }


    def testHasThisType2(): Unit = {
      def testSelf[PThis <: HasThisType[_ <: PThis]](that: PThis with HasThisType[PThis]): Unit = {
        // that.type <: that.This
        assert(implicitly[that.type <:< that.This] != null)
      }
      val that: HasThisType[_] = Foo() // null.asInstanceOf
      // this line of code makes Dotty compiler infinite recursion (stopped only by overflow) - comment it to make it compilable again
      testSelf(that) // error
    }

    // ---- ---- ---- ----

    // `HasThisType` instantiation/sub-classing
    trait FooLike[PThis <: FooLike[_ <: PThis]] extends HasThisType[PThis] {
      this: PThis =>
    }
    case class Foo(payload: Any = "dummy") extends FooLike[Foo]
    case class Bar(dummy: Any = "payload") extends FooLike[FooLike[_]]

  }
