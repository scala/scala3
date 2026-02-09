package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.FixMethodOrder
import org.junit.Ignore
import org.junit.Test
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class CompletionArgSuite extends BaseCompletionSuite:

  // In scala3, we get NoSymbol for `assert`, so we get no completions here.
  // This might be because the `assert` method has multiple overloaded methods, and that's why we can't retrieve a specfic symbol.
  // Might be good to fixed in Dotty.
  // see: https://github.com/scalameta/metals/pull/2369
  @Test def `arg` =
    check(
      s"""|object Main {
          |  assert(@@)
          |}
          |""".stripMargin,
      """|assertion = : Boolean
         |message = : => Any
         |""".stripMargin,
      topLines = Option(2)
    )

  @Test def `arg-newline` =
    check(
      s"""|object Main {
          |  def foo(banana: String, apple: String) = ???
          |  foo(
          |    @@
          |  )
          |}
          |""".stripMargin,
      """|apple = : String
         |banana = : String
         |""".stripMargin,
      topLines = Option(2)
    )

  @Test def `arg1` =
    check(
      s"""|object Main {
          |  assert(assertion = true, @@)
          |}
          |""".stripMargin,
      """|message = : => Any
         |Main test
         |""".stripMargin,
      topLines = Option(2)
    )

  @Test def `arg-edit` =
    checkEdit(
      s"""|object Main {
          |  assert(assertion = true, me@@)
          |}
          |""".stripMargin,
      """|object Main {
         |  assert(assertion = true, message = )
         |}
         |""".stripMargin
    )

  @Test def `arg2` =
    check(
      s"""|object Main {
          |  assert(true, @@)
          |}
          |""".stripMargin,
      """|message = : => Any
         |Main test
         |""".stripMargin,
      topLines = Option(2)
    )

  def user: String =
    """|case class User(
       |    name: String = "John",
       |    age: Int = 42,
       |    address: String = "",
       |    followers: Int = 0
       |)
       |""".stripMargin

  @Test def `arg3` =
    check(
      s"""|
          |$user
          |object Main {
          |  User("", address = "", @@)
          |}
          |""".stripMargin,
      """|age = : Int
         |followers = : Int
         |Main test
         |User(name: String = ..., age: Int = ..., address: String = ..., followers: Int = ...): User
         |""".stripMargin,
      topLines = Option(4)
    )

  // We should get NamedArg `address` from args in scala3, and remove `address` from completion, but it doesn't appear.
  // This might be good to fix in Dotty.
  // see: https://github.com/scalameta/metals/pull/2369
  @Test def `arg4` =
    check(
      s"""|
          |$user
          |object Main {
          |  User("", @@, address = "")
          |}
          |""".stripMargin,
      """|age = : Int
         |followers = : Int
         |Main test
         |""".stripMargin,
      topLines = Option(3)
    )

  @Test def `arg5` =
    check(
      s"""|
          |$user
          |object Main {
          |  User("", @@, address = "")
          |}
          |""".stripMargin,
      """|age = : Int
         |followers = : Int
         |Main test
         |User(name: String = ..., age: Int = ..., address: String = ..., followers: Int = ...): User
         |""".stripMargin,
      topLines = Option(4)
    )

  @Test def `arg6` =
    check(
      s"""|
          |$user
          |object Main {
          |  User("", @@ "")
          |}
          |""".stripMargin,
      """|address = : String
         |age = : Int
         |followers = : Int
         |""".stripMargin,
      topLines = Option(3)
    )

  @Test def `arg7` =
    check(
      s"""|
          |object Main {
          |  Option[Int](@@)
          |}
          |""".stripMargin,
      """|x = : A | Null
         |Main test
         |""".stripMargin,
      topLines = Option(2)
    )

  @Test def `arg8` =
    check(
      s"""|
          |object Main {
          |  "".stripSuffix(@@)
          |}
          |""".stripMargin,
      """|suffix = : String
         |Main test
         |""".stripMargin,
      topLines = Option(2)
    )

  // In scala3, we get NoSymbol for `until`, so we get no completions here.
  // This might be because the `1.until` method has multiple overloaded methods, like `until(end: Long)` and `until(start: Long, end: Long)`,
  // and that's why we can't retrieve a specfic symbol.
  // Might be good to fixed in Dotty.
  // see: https://github.com/scalameta/metals/pull/2369
  @Test def `arg9` =
    check(
      // `until` has multiple implicit conversion alternatives
      s"""|
          |object Main {
          |  1.until(@@)
          |}
          |""".stripMargin,
      """|`end` = : Int
         |Main test
         |""".stripMargin,
      topLines = Option(2)
    )

  @Test def `arg10` =
    check(
      s"""|$user
          |object Main {
          |  User(addre@@)
          |}
          |""".stripMargin,
      """|address = : String
         |""".stripMargin,
      topLines = Option(1)
    )

  @Test def `arg11` =
    check(
      s"""|object Main {
          |  def curry(a: Int)(banana: Int): Int = ???
          |  curry(1)(bana@@)
          |}
          |""".stripMargin,
      """|banana = : Int
         |""".stripMargin
    )

  @Test def `arg12` =
    check(
      s"""|object Main {
          |  def curry(a: Int)(banana: Int): Int = ???
          |  curry(bana@@)
          |}
          |""".stripMargin,
      ""
    )

  @Test def `arg13` =
    check(
      s"""|object Main {
          |  Array("")(evidence@@)
          |}
          |""".stripMargin,
      // assert that `evidence$1` is excluded.
      ""
    )

  @Test def `using` =
    checkEdit(
      s"""|def hello(using String): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  hello(st@@)
          |""".stripMargin,
      s"""|def hello(using String): Unit = ???
              |@main def main1(): Unit =
              |  val str = "hello"
              |  hello(using str)
              |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `using2` =
    checkEdit(
      s"""|def hello(using String): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  hello(using st@@)
          |""".stripMargin,
      s"""|def hello(using String): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  hello(using str)
          |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `using3` =
    checkEdit(
      s"""|def hello(using String, Int): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  val int = 4
          |  hello(str, in@@)
          |""".stripMargin,
      s"""|def hello(using String, Int): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  val int = 4
          |  hello(str, int)
          |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `using4` =
    checkEdit(
      s"""|def hello(name: String)(using String): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  hello("name")(str@@)
          |""".stripMargin,
      s"""|def hello(name: String)(using String): Unit = ???
          |@main def main1(): Unit =
          |  val str = "hello"
          |  hello("name")(using str)
          |""".stripMargin,
      assertSingleItem = false
    )

  @Test def `default-args` =
    check(
      s"""|object Main {
          |  def foo() = {
          |    def deployment(
          |      fst: Option[String],
          |      snd: Int = 1,
          |    ): Option[Int] = ???
          |    val abc = deployment(@@)
          |  }
          |}
          |""".stripMargin,
      """|fst = : Option[String]
         |snd = : Int
         |""".stripMargin,
      topLines = Some(2)
    )
  @Test def `default-args2` =
    check(
      s"""|object Main {
          |  def deployment(
          |    fst: Option[String],
          |    snd: Int = 1,
          |  ): Option[Int] = ???
          |  val abc = deployment(@@)
          |}
          |""".stripMargin,
      """|fst = : Option[String]
         |snd = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `default-args3` =
    check(
      s"""|object Main {
          |  def deployment(str: String)(
          |    fst: Option[String],
          |    snd: Int = 1,
          |  ): Option[Int] = ???
          |  val abc = deployment("str")(
          |    @@
          |  )
          |}
          |""".stripMargin,
      """|fst = : Option[String]
         |snd = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `default-args4` =
    check(
      s"""|object Main {
          |  def deployment(str: String)(opt: Option[Int])(
          |    fst: Option[String],
          |    snd: Int = 1,
          |  ): Option[Int] = ???
          |  val abc = deployment("str")(None)(
          |    @@
          |  )
          |}
          |""".stripMargin,
      """|fst = : Option[String]
         |snd = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `default-args5` =
    check(
      s"""|object Main {
          |  def deployment(str: String)(opt: Option[Int] = None)(
          |    fst: Option[String],
          |    snd: Int = 1,
          |  ): Option[Int] = ???
          |  val abc = deployment("str")(
          |    @@
          |  )
          |}
          |""".stripMargin,
      """|opt = : Option[Int]
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `default-args6` =
    check(
      s"""|object Main {
          |  def deployment(using str: String)(
          |    fst: Option[String],
          |    snd: Int = 1,
          |  ): Option[Int] = ???
          |  val abc = deployment(using "str")(
          |    @@
          |  )
          |}
          |""".stripMargin,
      """|fst = : Option[String]
         |snd = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  // @Test def `explicit-dollar` =
  // checkSnippet( // see: https://github.com/scalameta/metals/issues/2400
  //   """
  //     |object Main {
  //     |  def test($foo: Int, $bar: Int): Int = ???
  //     |  test($f@@)
  //     |}
  //     |""".stripMargin,
  //   """|$$foo = """.stripMargin,
  //   topLines = Option(1),
  // )

  // known issue: the second parameter with $ become | (returned from compiler)
  // see: https://github.com/scalameta/metals/issues/3690
  // @Test def `explicit-dollar-autofill` =
  //   checkSnippet(
  //     """
  //       |object Main {
  //       |  def test($foo: Int, $bar: Int): Int = ???
  //       |  test($f@@)
  //       |}
  //       |""".stripMargin,
  //     """|$$foo =
  //        |$$foo = ${1:???}, | = ${2:???}
  //        |""".stripMargin,
  //     topLines = Option(2),
  //     compat = Map(
  //       "3" -> """|$$foo =
  //                 |$$foo = ${1:???}, $$bar = ${2:???}
  //                 |""".stripMargin
  //     ),
  //   )

  @Test def `arg14` =
    check(
      s"""|object Main {
          |  val isLargeBanana = true
          |  processFile(isResourceFil@@)
          |  def processFile(isResourceFile: Boolean): Unit = ()
          |}
          |""".stripMargin,
      """|isResourceFile = : Boolean
         |isResourceFile = isLargeBanana : Boolean
         |""".stripMargin
    )

  @Test def `priority` =
    check(
      s"""|object Main {
          |  def foo(argument : Int) : Int = argument
          |  val argument = 5
          |  foo(argu@@)
          |}
          |""".stripMargin,
      """|argument: Int
         |argument = : Int
         |argument = argument : Int
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `priority-2` =
    check(
      s"""|case class A(argument: Int)
          |object Main {
          |  def foo(argument: Int): A =
          |    A(argu@@)
          |}
          |""".stripMargin,
      """|argument: Int
         |argument = : Int
         |argument = argument : Int
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `named-multiple` =
    check(
      s"""|object Main {
          |  def foo(argument : Int) : Int = argument
          |  val number = 1
          |  val number2 = 2
          |  val number4 = 4
          |  val number8 = 8
          |  foo(argu@@)
          |}
          |""".stripMargin,
      """|argument = : Int
         |argument = number : Int
         |argument = number2 : Int
         |argument = number4 : Int
         |argument = number8 : Int
         |""".stripMargin,
      topLines = Some(5)
    )

  @Test def `named-backticked` =
    check(
      s"""|object Main {
          |  def foo(`type` : Int) : Int = argument
          |  val number = 1
          |  val number2 = 2
          |  foo(ty@@)
          |}
          |""".stripMargin,
      """|`type` = : Int
         |`type` = number : Int
         |`type` = number2 : Int
         |""".stripMargin,
      topLines = Some(5)
    )

  @Test def `auto-no-show` =
    checkEditLine(
      s"""|object Main {
          |  def foo(argument : Int, other : String) : Int = argument
          |  val number = 5
          |  val hello = ""
          |  val relevant = 123
          |  ___
          |}
          |""".stripMargin,
      "foo(rele@@)",
      "foo(relevant)"
    )

  @Test def `auto` =
    checkEditLine(
      s"""|object Main {
          |  def foo(argument : Int, other : String) : Int = argument
          |  val number = 5
          |  val hello = ""
          |  ___
          |}
          |""".stripMargin,
      "foo(auto@@)",
      "foo(argument = ${1:number}, other = ${2:hello})"
    )

  @Test def `auto-inheritance` =
    checkEditLine(
      s"""|object Main {
          |  trait Animal
          |  class Dog extends Animal
          |
          |  trait Furniture
          |  class Chair extends Furniture
          |  def foo(animal: Animal, furniture: Furniture) : Int = 42
          |  val dog = new Dog()
          |  val chair = new Chair()
          |  ___
          |}
          |""".stripMargin,
      "foo(auto@@)",
      "foo(animal = ${1:dog}, furniture = ${2:chair})"
    )

  @Test def `auto-multiple-type` =
    checkEditLine(
      s"""|object Main {
          |  def foo(argument : Int, other : String, last : String = "") : Int = argument
          |  val number = 5
          |  val argument = 123
          |  val hello = ""
          |  ___
          |}
          |""".stripMargin,
      "foo(auto@@)",
      "foo(argument = ${1|???,argument,number|}, other = ${2:hello})"
    )

  @Test def `auto-not-found` =
    checkEditLine(
      s"""|object Main {
          |  val number = 234
          |  val nothing = throw new Exception
          |  val nll = null
          |  def foo(argument : Int, other : String, isTrue: Boolean, opt : Option[String]) : Int = argument
          |  ___
          |}
          |""".stripMargin,
      "foo(auto@@)",
      "foo(argument = ${1:number}, other = ${2:???}, isTrue = ${3:???}, opt = ${4:???})"
    )

  @Test def `auto-list` =
    checkEditLine(
      s"""|object Main {
          |  def foo(argument : List[String], other : List[Int]) : Int = 0
          |  val list1 = List(1,2,3)
          |  val list2 = List(3,2,1)
          |  val list3 = List("")
          |  val list4 = List("")
          |  ___
          |}
          |""".stripMargin,
      "foo(auto@@)",
      "foo(argument = ${1|???,list3,list4|}, other = ${2|???,list1,list2|})"
    )

  @Test def `wrap-idents` =
    checkEditLine(
      s"""|object Main {
          |  def f(a: String, b: String, `type`: String) = a + b + `type`
          |  val str = ""
          |  val str1 = ""
          |  ___
          |}
          |""".stripMargin,
      "f(auto@@)",
      "f(a = ${1|???,str,str1|}, b = ${2|???,str,str1|}, `type` = ${3|???,str,str1|})"
    )

  @Test def `nested-apply` =
    check(
      s"""|object Main{
          |  def foo(argument1: Int, argument2: Int): Int = argument1 + argument2
          |  val x: Int = 3
          |  foo(foo(@@), )
          |}
          |""".stripMargin,
      """|argument1 = : Int
         |argument1 = x : Int
         |argument2 = : Int
         |argument2 = x : Int
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `infix` =
    check(
      s"""|object Main{
          |  val lst: List[Int] = List(1, 2, 3)
          |  lst.map(x => x * x@@ )
          |}
          |""".stripMargin,
      """|x: Int
         |""".stripMargin
    )

  @Test def `constructor-param` =
    check(
      """|class Foo (xxx: Int)
         |
         |object Main {
         |  val foo = new Foo(x@@)
         |}
         |""".stripMargin,
      """|xxx = : Int
         |""".stripMargin
    )

  @Test def `constructor-param2` =
    check(
      """|class Foo ()
         |
         |object Foo {
         |  def apply(xxx: Int): Foo = ???
         |}
         |object Main {
         |  val foo = Foo(x@@)
         |}
         |""".stripMargin,
      """|xxx = : Int
         |""".stripMargin
    )

  @Test def `context-function-as-param` =
    check(
      s"""|case class Context()
          |
          |object Main:
          |  def foo(arg1: (Context) ?=> Int, arg2: Int): String = ???
          |  val m = foo(ar@@)
          |""".stripMargin,
      """|arg1 = : (Context) ?=> Int
         |arg2 = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `context-function-as-param2` =
    check(
      s"""|case class Context()
          |
          |object Main:
          |  def foo(arg1: Context ?=> Int, arg2: Context ?=> Int): String = ???
          |  val m = foo(arg1 = ???, a@@)
          |""".stripMargin,
      """|arg2 = : (Context) ?=> Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `context-function-as-param3` =
    check(
      s"""|case class Context()
          |
          |object Main:
          |  def foo(arg1: (Boolean, Context) ?=> Int ?=> String, arg2: (Boolean, Context) ?=> Int ?=> String): String = ???
          |  val m = foo(arg1 = ???, a@@)
          |""".stripMargin,
      """|arg2 = : (Boolean, Context) ?=> (Int) ?=> String
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `1-second-arg-first` =
    check(
      """|case class Test(
         |    testA: String,
         |    testB: Option[String],
         |    testC: String,
         |)
         |object Main {
         |  def test(x: Test) = {
         |    x.copy(testB = ???, te@@)
         |  }
         |}
         |""".stripMargin,
      """|testA = : String
         |testC = : String
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `case-class-apply` =
    check(
      """|object Main {
         |  def m() = {
         |    case class A(foo: Int, fooBar: Int)
         |    println(A(foo@@))
         |  }
         |}
         |""".stripMargin,
      """|foo = : Int
         |fooBar = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `case-class-apply1` =
    check(
      """|object Main {
         |  def m() = {
         |    case class A(foo: Int, fooBar: Int)
         |    object A { def apply(foo: Int) = new A(foo, 3) }
         |    println(A(foo@@))
         |  }
         |}
         |""".stripMargin,
      """|foo = : Int
         |fooBar = : Int
         |""".stripMargin
    )

  @Test def `case-class-apply2` =
    check(
      """|object Main {
         |  def m() = {
         |    case class A(foo: Int, fooBar: Int)
         |    object A { def apply(foo: Int) = new A(foo, 3) }
         |    println(A(foo = 1, foo@@))
         |  }
         |}
         |""".stripMargin,
      """|fooBar = : Int
         |""".stripMargin
    )

  @Test def `case-class-apply3` =
    check(
      """|case class A(val foo: Int, val fooBar: Int)
         |object A {
         |  def apply(foo: Int): A = new A(foo, 3)
         |}
         |
         |object Main {
         |  for {
         |      a <- List(1, 2, 3)
         |      x = A(foo@@)
         |   }
         |}
         |""".stripMargin,
      """|foo = : Int
         |foo = a : Int
         |fooBar = : Int
         |fooBar = a : Int
         |""".stripMargin
    )

  @Test def `case-class-apply4` =
    check(
      """|case class A(val foo: Int, val fooBar: Int)
         |object A {
         |  def apply(foo: Int): A = new A(foo, 3)
         |}
         |
         |object Main {
         |  for {
         |      a <- List(1, 2, 3)
         |      x = A(foo = 1, foo@@)
         |   }
         |}
         |""".stripMargin,
      """|fooBar = : Int
         |fooBar = a : Int
         |""".stripMargin
    )

  @Test def `case-class-for-comp` =
    check(
      """|case class Abc(foo: Int, fooBar: Int)
         |object Main {
         |   for {
         |      a <- List(1, 2, 3)
         |      x = Abc(foo@@)
         |   }
         |}
         |""".stripMargin,
      """|foo = : Int
         |foo = a : Int
         |fooBar = : Int
         |fooBar = a : Int
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `recursive` =
    check(
      """|
         |object Main {
         |   def foo(value: Int): Int = {
         |     foo(valu@@)
         |   }
         |}
         |""".stripMargin,
      """|value = : Int
         |value = value : Int
         |value: Int
         |""".stripMargin,
      topLines = Some(4)
    )

  @Test def `overloaded-with-param` =
    check(
      """|object Main:
         |  def m(idd : String, abb: Int): Int = ???
         |  def m(inn : Int, uuu: Option[Int]): Int = ???
         |  def m(inn : Int, aaa: Int): Int = ???
         |  def k: Int = m(1, a@@)
         |""".stripMargin,
      """|aaa = : Int
         |assert(inline assertion: Boolean): Unit
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `overloaded-with-named-param` =
    check(
      """|object Main:
         |  def m(idd : String, abb: Int): Int = ???
         |  def m(inn : Int, uuu: Option[Int]): Int = ???
         |  def m(inn : Int, aaa: Int): Int = ???
         |  def k: Int = m(inn = 1, a@@)
         |""".stripMargin,
      """|aaa = : Int
         |assert(inline assertion: Boolean): Unit
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `overloaded-generic` =
    check(
      """|object Main:
         |  val g = 3
         |  val l : List[Int] = List(1,2,3)
         |  def m[T](inn : List[T], yy: Int, aaa: Int, abb: Option[Int]): Int = ???
         |  def m[T](inn : List[T], yy: Int, aaa: Int, abb: Int): Int = ???
         |  def k: Int = m(yy = 3, inn = l, a@@)
         |""".stripMargin,
      """|aaa = : Int
         |aaa = g : Int
         |abb = : Int
         |abb = : Option[Int]
         |abb = g : Int
         |""".stripMargin,
      topLines = Some(5)
    )

  @Test def `overloaded-methods` =
    check(
      """|class A():
         |  def m(anInt : Int): Int = ???
         |  def m(aString : String): String = ???
         |
         |object O:
         |  def m(aaa: Int): Int = ???
         |  val k = new A().m(a@@)
         |""".stripMargin,
      """|aString = : String
         |anInt = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `overloaded-methods2` =
    check(
      """|class A():
         |  def m(anInt : Int): Int = ???
         |  def m(aString : String): String = ???
         |  private def m(aBoolean: Boolean): Boolean = ???
         |
         |object O:
         |  def m(aaa: Int): Int = ???
         |  val myInstance = new A()
         |  val k = myInstance.m(a@@)
         |""".stripMargin,
      """|aString = : String
         |anInt = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `overloaded-select` =
    check(
      """|package a.b {
         |  object A {
         |    def m(anInt : Int): Int = ???
         |    def m(aString : String): String = ???
         |  }
         |}
         |object O {
         |  def m(aaa: Int): Int = ???
         |  val k = a.b.A.m(a@@)
         |}
         |""".stripMargin,
      """|aString = : String
         |anInt = : Int
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `overloaded-in-a-class` =
    check(
      """|trait Planet
         |case class Venus() extends Planet
         |class Main[T <: Planet](t : T) {
         |  def m(inn: Planet, abb: Option[Int]): Int = ???
         |  def m(inn: Planet, aaa: Int): Int = ???
         |  def k = m(t, a@@)
         |}
         |""".stripMargin,
      """|aaa = : Int
         |abb = : Option[Int]
         |""".stripMargin,
      topLines = Some(2)
    )

  @Test def `overloaded-function-param` =
    check(
      """|object Main:
         |  def m[T](i: Int)(inn: T => Int, abb: Option[Int]): Int = ???
         |  def m[T](i: Int)(inn: T => Int, aaa: Int): Int = ???
         |  def m[T](i: Int)(inn: T => String, acc: List[Int]): Int = ???
         |  def k = m(1)(inn = identity[Int], a@@)
         |""".stripMargin,
      """|aaa = : Int
         |abb = : Option[Int]
         |assert(inline assertion: Boolean): Unit
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `overloaded-function-param2` =
    check(
      """|object Main:
         |  def m[T](i: Int)(inn: T => Int, abb: Option[Int]): Int = ???
         |  def m[T](i: Int)(inn: T => Int, aaa: Int): Int = ???
         |  def m[T](i: String)(inn: T => Int, acc: List[Int]): Int = ???
         |  def k = m(1)(inn = identity[Int], a@@)
         |""".stripMargin,
      """|aaa = : Int
         |abb = : Option[Int]
         |assert(inline assertion: Boolean): Unit
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `overloaded-applied-type` =
    check(
      """|trait MyCollection[+T]
         |case class IntCollection() extends MyCollection[Int]
         |object Main {
         |  def m[T](inn: MyCollection[T], abb: Option[Int]): Int = ???
         |  def m[T](inn: MyCollection[T], aaa: Int): Int = ???
         |  def m[T](inn: List[T], acc: Int): Int = ???
         |  def k = m(IntCollection(), a@@)
         |}
         |""".stripMargin,
      """|aaa = : Int
         |abb = : Option[Int]
         |assert(inline assertion: Boolean): Unit
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `overloaded-bounds` =
    check(
      """|trait Planet
         |case class Moon()
         |object Main {
         |  def m[M](inn: M, abb: Option[Int]): M = ???
         |  def m[M](inn: M, acc: List[Int]): M = ???
         |  def m[M <: Planet](inn: M, aaa: Int): M = ???
         |  def k = m(Moon(), a@@)
         |}
         |""".stripMargin,
      """|abb = : Option[Int]
         |acc = : List[Int]
         |assert(inline assertion: Boolean): Unit
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `overloaded-or-type` =
    check(
      """|object Main:
         |  val h : Int = 3
         |  def m[T](inn: String | T, abb: Option[Int]): Int = ???
         |  def m(inn: Int, aaa: Int): Int = ???
         |  def k: Int = m(3, a@@)
         |""".stripMargin,
      """|aaa = : Int
         |aaa = h : Int
         |abb = : Option[Int]
         |""".stripMargin,
      topLines = Some(3)
    )

  @Test def `overloaded-function-param3` =
    check(
      """|object Main:
         |  def m[T](inn: Int => T, abb: Option[Int]): Int = ???
         |  def m[T](inn: String => T, aaa: Int): Int = ???
         |  def k = m(identity[Int], a@@)
         |""".stripMargin,
      """|abb = : Option[Int]
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `lambda` =
    check(
      """|val hello: (x: Int) => Unit = x => println(x)
         |val k = hello(@@)
         |""".stripMargin,
      """|x = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `lambda2` =
    check(
      """|object O:
         |  val hello: (x: Int, y: Int) => Unit = (x, _) => println(x)
         |val k = O.hello(x = 1, @@)
         |""".stripMargin,
      """|y = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `lambda3` =
    check(
      """|val hello: (x: Int) => (j: Int) => Unit = x => j => println(x)
         |val k = hello(@@)
         |""".stripMargin,
      """|x = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `lambda4` =
    check(
      """|val hello: (x: Int) => (j: Int) => (str: String) => Unit = x => j => str => println(str)
         |val k = hello(x = 1)(2)(@@)
         |""".stripMargin,
      """|str = : String
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `lambda5` =
    check(
      """|val hello: (x: Int) => Int => (str: String) => Unit = x => j => str => println(str)
         |val k = hello(x = 1)(2)(@@)
         |""".stripMargin,
      """|str = : String
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `second-first` =
    check(
      """|object Main {
         |  def foo(aaa: Int, bbb: Int, ccc: Int) = aaa + bbb + ccc
         |  val k = foo (
         |    bbb = 123,
         |    aa@@
         |  )
         |}
         |""".stripMargin,
      """|aaa = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `second-first2` =
    check(
      """|object Main {
         |  def foo(aaa: Int, bbb: Int, ccc: Int) = aaa + bbb + ccc
         |  val k = foo (
         |    bbb = 123,
         |    ccc = 123,
         |    aa@@
         |  )
         |}
         |""".stripMargin,
      """|aaa = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `second-first3` =
    check(
      """|object Main {
         |  def foo(ddd: Int)(aaa: Int, bbb: Int, ccc: Int) = aaa + bbb + ccc
         |  val k = foo(123)(
         |    bbb = 123,
         |    ccc = 123,
         |    aa@@
         |  )
         |}
         |""".stripMargin,
      """|aaa = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `second-first4` =
    check(
      """|object O:
         |  val hello: (x: Int, y: Int) => Unit = (x, _) => println(x)
         |val k = O.hello(y = 1, @@)
         |""".stripMargin,
      """|x = : Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `second-first5` =
    check(
      """|val hello: (x: Int) => Int => (str: String, ccc: String) => Unit = x => j => (str, _) => println(str)
         |val k = hello(x = 1)(2)(ccc = "abc", @@)
         |""".stripMargin,
      """|str = : String
         | """.stripMargin,
      topLines = Some(1)
    )

  @Ignore
  @Test def `comparison` =
    check(
      """
        |object w {
        |  abstract class T(x: Int) {
        |    def met(x: Int): Unit = {
        |      println(x@@)
        |    }
        |  }}
        |""".stripMargin,
      """x: Int
        |x = : Any""".stripMargin
    )

  @Test def `autofill-arguments-case-class` =
    check(
      """
        |case class A(x: Int, y: Int)
        |
        |def main() =
        |  A(x@@)
        |""".stripMargin,
      """x = : Int
        |x = ???, y = ???""".stripMargin
      // this looks strange due to the Autofill message belonging to the description
    )
