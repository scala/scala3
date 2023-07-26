package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.FixMethodOrder
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
         |Main test
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
         |User test
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
         |User test
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
      """|x = : A
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

  @Test def `contructor-param` =
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

  @Test def `contructor-param2` =
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
          |def foo(arg1: (Context) ?=> Int, arg2: Int): String = ???
          |val m = foo(ar@@)
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
          |def foo(arg1: Context ?=> Int, arg2: Context ?=> Int): String = ???
          |val m = foo(arg1 = ???, a@@)
          |""".stripMargin,
      """|arg2 = : (Context) ?=> Int
         |""".stripMargin,
      topLines = Some(1)
    )

  @Test def `context-function-as-param3` =
    check(
      s"""|case class Context()
          |
          |def foo(arg1: (Boolean, Context) ?=> Int ?=> String, arg2: (Boolean, Context) ?=> Int ?=> String): String = ???
          |val m = foo(arg1 = ???, a@@)
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
      topLines = Some(4),
    )

