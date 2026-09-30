object Main {
    def main(a: Array[String]): Unit = {
      println("you may not run `testHasThisType` - just check that it compiles")
      // comment lines after "// this line of code makes" comments to make it compilable again
      testHasThisType()
    }

    // ---- ---- ---- ----

    trait HasThisType[PThis <: HasThisType[_ <: PThis]] {
      this: PThis =>
      type This = PThis

      // inline // uncommenting `inline` cause problem in scastie dotty version, but is fixed in dotty `master`
      def self(): This & this.type = this
    }

    // ---- ---- ---- ----

    def testHasThisType(): Unit = {
      def testSelf[PThis <: HasThisType[_ <: PThis]](that: HasThisType[PThis]): Unit = {
        val thatSelf = that.self()
        // that.self().type <: that.This
        assert(implicitly[thatSelf.type <:< that.This] != null)
      }
      val that: HasThisType[_] = Foo() // null.asInstanceOf
      testSelf(that) // error: recursion limit exceeded
    }

    // ---- ---- ---- ----

    // `HasThisType` instantiation/sub-classing
    trait FooLike[PThis <: FooLike[_ <: PThis]] extends HasThisType[PThis] {
      this: PThis =>
    }
    case class Foo(payload: Any = "dummy") extends FooLike[Foo]
    case class Bar(dummy: Any = "payload") extends FooLike[FooLike[_]]

  }
