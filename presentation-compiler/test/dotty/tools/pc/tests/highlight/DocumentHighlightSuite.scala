package dotty.tools.pc.tests.highlight

import dotty.tools.pc.base.BaseDocumentHighlightSuite

import org.junit.Test

class DocumentHighlightSuite extends BaseDocumentHighlightSuite:

  @Test def `single` =
    check(
      """
        |object Main {
        |  Option(1).<<he@@ad>>
        |}""".stripMargin
    )

  @Test def `multiple` =
    check(
      """
        |object Main {
        |  val <<abc>> = 123
        |  <<abc>>.toInt
        |  println(<<ab@@c>>)
        |}""".stripMargin
    )

  @Test def `multiple2` =
    check(
      """
        |object Main {
        |  val <<a@@bc>> = 123
        |  <<abc>>.toInt
        |  println(<<abc>>)
        |}""".stripMargin
    )

  @Test def `multiple3` =
    check(
      """
        |object Main {
        |  val <<abc>> = 123
        |  <<ab@@c>>.toInt
        |  println(<<abc>>)
        |}""".stripMargin
    )

  @Test def `different-symbols` =
    check(
      """
        |object Main {
        |  val abc = 123
        |  abc.<<to@@Int>>
        |  134l.toInt
        |}""".stripMargin
    )

  @Test def `scopes` =
    check(
      """
        |object Main {
        |  val <<@@a>> = 123
        |  val f = (a: Int) => a + 1
        |  println(<<a>>)
        |}""".stripMargin
    )

  @Test def `scopes2` =
    check(
      """
        |object Main {
        |  val <<a>> = 123
        |  val f = (a: Int) => a + 1
        |  println(<<@@a>>)
        |}""".stripMargin
    )

  @Test def `params` =
    check(
      """
        |case class User(<<n@@ame>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `params2` =
    check(
      """
        |case class User(<<name>>: String)
        |object Main {
        |  val user = User(<<na@@me>> = "Susan")
        |  println(user.<<name>>)
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `params3` =
    check(
      """
        |case class User(<<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<n@@ame>>)
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `params4` =
    check(
      """
        |case class User(<<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.copy(<<na@@me>> = "John")
        |}""".stripMargin
    )

  @Test def `object` =
    check(
      """
        |case class <<U@@ser>>(name: String)
        |object <<User>>
        |object Main {
        |  val user = <<User>>(name = "Susan")
        |  println(user.name)
        |  user.copy(name = "John")
        |}""".stripMargin
    )

  @Test def `object2` =
    check(
      """
        |case class <<User>>(name: String)
        |object <<Us@@er>>
        |object Main {
        |  val user = <<User>>(name = "Susan")
        |  println(user.name)
        |  user.copy(name = "John")
        |}""".stripMargin
    )

  @Test def `object3` =
    check(
      """
        |case class <<User>>(name: String)
        |object <<User>>
        |object Main {
        |  val user = <<U@@ser>>(name = "Susan")
        |  println(user.name)
        |  user.copy(name = "John")
        |}""".stripMargin
    )

  @Test def `case-class-var` =
    check(
      """
        |case class User(var <<na@@me>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<name>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `case-class-var2` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<na@@me>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<name>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `case-class-var3` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<n@@ame>>)
        |  user.<<name>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `case-class-var4` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<na@@me>> = ""
        |  user.copy(<<name>> = "John")
        |}""".stripMargin
    )

  @Test def `case-class-var5` =
    check(
      """
        |case class User(var <<name>>: String)
        |object Main {
        |  val user = User(<<name>> = "Susan")
        |  println(user.<<name>>)
        |  user.<<name>> = ""
        |  user.copy(<<na@@me>> = "John")
        |}""".stripMargin
    )

  @Test def `var` =
    check(
      """
        |object Main {
        |  var <<ab@@d>> = 123
        |  <<abd>> = 344
        |  <<abd>> +=1
        |  println(<<abd>>)
        |}""".stripMargin
    )

  @Test def `var2` =
    check(
      """
        |object Main {
        |  var <<abd>> = 123
        |  <<ab@@d>> = 344
        |  <<abd>> +=1
        |  println(<<abd>>)
        |}""".stripMargin
    )

  @Test def `var3` =
    check(
      """
        |object Main {
        |  var <<abd>> = 123
        |  <<abd>> = 344
        |  <<ab@@d>> +=1
        |  println(<<abd>>)
        |}""".stripMargin
    )

  @Test def `var4` =
    check(
      """
        |object Main {
        |  var <<abd>> = 123
        |  <<abd>> = 344
        |  <<abd>> +=1
        |  println(<<a@@bd>>)
        |}""".stripMargin
    )

  @Test def `overloaded` =
    check(
      """
        |object Main {
        |  def hello() = ""
        |  def <<hel@@lo>>(a : Int) = ""
        |  def hello(a : Int, b : String) = ""
        |}""".stripMargin
    )

  @Test def `local-var` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<a@@bc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-var2` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<ab@@c>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-var3` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<a@@bc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-assign` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<ab@@c>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-assign2` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<a@@bc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-assign3` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<a@@bc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-class` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<ab@@c>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-class2` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<a@@bc>> = 4
        |          def m3: Int = <<abc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `local-class3` =
    check(
      """
        |object Test {
        |  def met() = {
        |    class T1(var abc: Int) {
        |       class T2(var <<abc>>: Int) {
        |          <<abc>> = 4
        |          def m3: Int = <<a@@bc>> + 2
        |      }
        |      abc = 4
        |      def m2: Int = abc + 2
        |    }
        |  }
        |}""".stripMargin
    )

  @Test def `setter-getter` =
    check(
      """
        |object Test {
        |  class T1{
        |    def <<ar@@g_=>> (arg: Int) = {}
        |    def <<arg>> = 1
        |  }
        |  val t = new T1
        |  t.<<arg>> = 123
        |}""".stripMargin
    )

  @Test def `setter-getter2` =
    check(
      """
        |object Test {
        |  class T1{
        |    def <<arg_=>> (arg: Int) = {}
        |    def <<a@@rg>> = 1
        |  }
        |  val t = new T1
        |  t.<<arg>> = 123
        |
        |}""".stripMargin
    )

  @Test def `setter-getter3` =
    check(
      """
        |object Test {
        |  class T1{
        |    def <<arg_=>> (arg: Int) = {}
        |    def <<arg>> = 1
        |  }
        |  val t = new T1
        |  t.<<ar@@g>> = 123
        |}""".stripMargin
    )

  @Test def `same-name` =
    check(
      """
        |object Test {
        |  def foo(name: String) = ???
        |  def bar(<<n@@ame>>: String) = ???
        |  foo(name = "123")
        |  bar(<<name>> = "123")
        |}""".stripMargin
    )

  @Test def `same-name2` =
    check(
      """
        |object Test {
        |  def foo(name: String) = ???
        |  def bar(<<name>>: String) = ???
        |  foo(name = "123")
        |  bar(<<na@@me>> = "123")
        |}""".stripMargin
    )

  @Test def `same-name3` =
    check(
      """
        |object Test {
        |  def foo(<<na@@me>>: String) = ???
        |  def bar(name: String) = ???
        |  foo(<<name>> = "123")
        |  bar(name = "123")
        |}""".stripMargin
    )

  @Test def `same-name4` =
    check(
      """
        |object Test {
        |  def foo(<<name>>: String) = ???
        |  def bar(name: String) = ???
        |  foo(<<na@@me>> = "123")
        |  bar(name = "123")
        |}""".stripMargin
    )

  @Test def `import1` =
    check(
      """
        |import scala.util.<<Tr@@y>>
        |object Test {
        |   <<Try>>(1)
        |}""".stripMargin
    )

  @Test def `import2` =
    check(
      """
        |import scala.util.<<Try>>
        |object Test {
        |   <<Tr@@y>>(1)
        |}""".stripMargin
    )

  @Test def `import3` =
    check(
      """
        |import scala.<<ut@@il>>.Try
        |object Test {
        |   scala.<<util>>.Try(1)
        |}""".stripMargin
    )

  @Test def `import4` =
    check(
      """
        |import scala.<<util>>.Try
        |object Test {
        |   scala.<<ut@@il>>.Try(1)
        |}""".stripMargin
    )

  @Test def `rename1` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATr@@y>>}
        |object Test {
        |   <<ATry>>(1)
        |}""".stripMargin
    )

  @Test def `rename2` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATry>>}
        |object Test {
        |   <<ATr@@y>>(1)
        |}""".stripMargin
    )

  // @note, we could try and not highlight normal Try,
  // but this might still be useful
  @Test def `rename3` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATr@@y>>}
        |object Test {
        |   scala.util.<<Try>>(1)
        |}""".stripMargin
    )

  @Test def `rename4` =
    check(
      """
        |import scala.util.{ <<Try>> => <<ATry>>}
        |object Test {
        |   scala.util.<<Tr@@y>>(1)
        |}""".stripMargin
    )

  @Test def `rename5` =
    check(
      """
        |import scala.util.{ <<T@@ry>> => <<ATry>>}
        |object Test {
        |   scala.util.<<Try>>(1)
        |}""".stripMargin
    )

  @Test def `case-match1` =
    check(
      """
        |import scala.util.Try
        |import scala.util.Success
        |object Test {
        |   Try(1) match {
        |     case Success(<<va@@lue>>) =>
        |       <<value>>
        |   }
        |}""".stripMargin
    )

  @Test def `case-match2` =
    check(
      """
        |import scala.util.Try
        |import scala.util.Success
        |object Test {
        |   Try(1) match {
        |     case Success(<<value>>) =>
        |       <<va@@lue>>
        |   }
        |}""".stripMargin
    )

  @Test def `inner-class1` =
    check(
      """|object Main {
         |  def foo = {
         |    case class <<U@@ser>>(name: String)
         |    object <<User>>{ def nnn = ""}
         |    <<User>>.nnn
         |  }
         |}""".stripMargin
    )

  @Test def `inner-class2` =
    check(
      """|object Main {
         |  def foo = {
         |    case class <<User>>(name: String)
         |    object <<U@@ser>>{ def nnn = ""}
         |    <<User>>.nnn
         |  }
         |}""".stripMargin
    )

  @Test def `inner-class3` =
    check(
      """|object Main {
         |  def foo = {
         |    case class <<User>>(name: String)
         |    object <<User>>{ def nnn = ""}
         |    <<Use@@r>>.nnn
         |  }
         |}""".stripMargin
    )

  @Test def `inner-class4` =
    check(
      """|object Main {
         |  def foo = {
         |    object O {
         |      case class <<User>>(name: String)
         |      object <<User>>{ def nnn = ""}
         |      <<Use@@r>>.nnn
         |    }
         |  }
         |}""".stripMargin
    )

  @Test def `package-object` =
    check(
      """|package example
         |
         |package object <<nes@@ted>> {
         |
         |  class PackageObjectNestedClass
         |
         |}
         |""".stripMargin
    )

  @Test def `named-param` =
    check(
      """|object Main {
         |  def foo = {
         |      case class User(<<name>>: String)
         |      val a = User(<<na@@me>> = "abc")
         |  }
         |}""".stripMargin
    )

  @Test def `backtick` =
    check(
      """|object Main {
         |  val <<`hi-!`>> = 5
         |
         |  <<`hi@@-!`>> + 3
         |}""".stripMargin
    )

  @Test def `shadowing` =
    check(
      """|object Main {
         |  val abc = {
         |    val <<abc>> = 1
         |    <<a@@bc>> + 1
         |  }
         |  val d = abc + 1
         |}""".stripMargin
    )

  @Test def `select-parentheses` =
    check(
      """|object Main {
         |  val a = (1 + 2 + 3).<<toStr@@ing>>
         |}""".stripMargin
    )

  @Test def `select-parentheses2` =
    check(
      """|object Main {
         |  val a = (1 + 2 + 3) <<:@@:>> Nil
         |}""".stripMargin
    )

  @Test def `trailling-comma` =
    check(
      """
        |object Main {
        |  val a = 1
        |  val <<b>> = 2
        |  List(
        |    a,
        |    <<b@@>>,
        |  )
        |}""".stripMargin
    )

  @Test def `trailling-comma2` =
    check(
      """
        |object Main {
        |  val a = 1
        |  val <<`ab`>> = 2
        |  List(
        |    a,
        |    <<`ab@@`>>,
        |  )
        |}""".stripMargin
    )

  @Test def `for-comp-bind` =
    check(
      """
        |object Main {
        |  case class Bar(fooBar: Int, goo: Int)
        |  val abc = for {
        |    foo <- List(1)
        |    _ = Option(1)
        |    Bar(<<fooBar>>, goo) <- List(Bar(foo, 123))
        |    baz = <<fooBar>> + goo
        |  } yield {
        |    val x = foo + <<foo@@Bar>> + baz
        |    x
        |  }
        |}""".stripMargin
    )

  @Test def `for-comp-map` =
    check(
      """|object Main {
         |  val x = List(1).<<m@@ap>>(_ + 1)
         |  val y = for {
         |    a <- List(1)
         |  } yield a + 1
         |}
         |""".stripMargin
    )

  @Test def `for-comp-map1` =
    check(
      """|object Main {
         |  val x = List(1).<<m@@ap>>(_ + 1)
         |  val y = for {
         |    a <- List(1)
         |    if true
         |  } yield a + 1
         |}
         |""".stripMargin
    )

  @Test def `for-comp-foreach` =
    check(
      """|object Main {
         |  val x = List(1).<<for@@each>>(_ => ())
         |  val y = for {
         |    a <- List(1)
         |  } {}
         |}
         |""".stripMargin
    )

  @Test def `for-comp-withFilter` =
    check(
      """|object Main {
         |  val x = List(1).<<with@@Filter>>(_ => true)
         |  val y = for {
         |    a <- List(1)
         |    if true
         |  } {}
         |}
         |""".stripMargin
    )

  @Test def `for-comp-withFilter1` =
    check(
      """|object Main {
         |  val x = List(1).withFilter(_ => true).<<m@@ap>>(_ + 1)
         |  val y = for {
         |    a <- List(1)
         |    if true
         |  } yield a + 1
         |}
         |""".stripMargin
    )

  @Test def `for-comp-flatMap1` =
    check(
      """|object Main {
         |  val x = List(1).<<flat@@Map>>(_ => List(1))
         |  val y = for {
         |    a <- List(1)
         |    b <- List(2)
         |    if true
         |  } yield a + 1
         |}
         |""".stripMargin
    )

  @Test def `for-comp-flatMap2` =
    check(
      """|object Main {
         |  val x = List(1).withFilter(_ => true).<<flat@@Map>>(_ => List(1))
         |  val y = for {
         |    a <- List(1)
         |    if true
         |    b <- List(2)
         |  } yield a + 1
         |}
         |""".stripMargin
    )

  @Test def `named-arg-backtick` =
    check(
      """|object Main {
           |  def foo(<<`type`>>: String): String = <<`type`>>
           |  val x = foo(
           |    <<`ty@@pe`>> = "abc"
           |  )
           |}
           |""".stripMargin
    )

  @Test def `enum1` =
    check(
      """|enum FooEnum:
         |  case <<Ba@@r>>, Baz
         |val bar = FooEnum.<<Bar>>
         |""".stripMargin
    )

  @Test def `enum2` =
    check(
      """|enum FooEnum:
         |  case <<Bar>>, Baz
         |val bar = FooEnum.<<Ba@@r>>
         |""".stripMargin
    )

  @Test def `transparent1` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def <<foo>>(i: Int): Foo = new Bar
         |val iii = 123
         |val bar = <<f@@oo>>(iii)
         |""".stripMargin
    )

  @Test def `transparent2` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def <<f@@oo>>(i: Int): Foo = new Bar
         |val iii = 123
         |val bar = <<foo>>(iii)
         |""".stripMargin
    )

  @Test def `transparent3` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def foo(i: Int): Foo = new Bar
         |val <<ii@@i>> = 123
         |val bar = foo(<<iii>>)
         |""".stripMargin
    )

  @Test def `transparent4` =
    check(
      """|trait Foo
         |class Bar extends Foo
         |
         |transparent inline def foo(i: Int): Foo = new Bar
         |val <<iii>> = 123
         |val bar = foo(<<i@@ii>>)
         |""".stripMargin
    )

  @Test def `recursive-inline1` =
    check(
      """|inline def <<po@@wer>>(x: Double, n: Int): Double =
         |  if n == 0 then 1.0
         |  else if n == 1 then x
         |  else
         |    val y = <<power>>(x, n / 2)
         |    if n % 2 == 0 then y * y else y * y * x
         |""".stripMargin
    )

  @Test def `recursive-inline2` =
    check(
      """|inline def <<power>>(x: Double, n: Int): Double =
         |  if n == 0 then 1.0
         |  else if n == 1 then x
         |  else
         |    val y = <<po@@wer>>(x, n / 2)
         |    if n % 2 == 0 then y * y else y * y * x
         |""".stripMargin
    )

  @Test def `extension-params` =
    check(
      """|extension (<<sb@@d>>: String)
         |  def double = <<sbd>> + <<sbd>>
         |  def double2 = <<sbd>> + <<sbd>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-params-ref` =
    check(
      """|extension (<<sbd>>: String)
         |  def double = <<sb@@d>> + <<sbd>>
         |  def double2 = <<sbd>> + <<sbd>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-type-param` =
    check(
      """|extension [T](<<x@@s>>: List[T])
         |  def double = <<xs>> ++ <<xs>>
         |  def double2 = <<xs>> ++ <<xs>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-type-param-ref` =
    check(
      """|extension [T](<<xs>>: List[T])
         |  def double = <<xs>> ++ <<xs>>
         |  def double2 = <<xs>> ++ <<x@@s>>
         |end extension
         |""".stripMargin
    )

  @Test def `extension-with-type` =
    check(
      """|object Mincase:
         |  extension [X](x: X)
         |    def <<foobar>>(): Unit = ???
         |
         |  val x = 1.<<foo@@bar>>()
         |  val y = (1: Int).<<foobar>>()
         |""".stripMargin
    )

  @Test def `extension-complex` =
    check(
      """|object Extensions:
         |
         |  extension [A, B](<<eit@@hers>>: Seq[Either[A, B]])
         |    def sequence = <<eithers>>.partitionMap(identity) match
         |      case (Nil, rights)       => Right(rights)
         |      case (firstLeft :: _, _) => Left(firstLeft)
         |    def sequence2 = <<eithers>>.partitionMap(identity) match
         |      case (Nil, rights)       => Right(rights)
         |      case (firstLeft :: _, _) => Left(firstLeft)
         |
         |  extension (map: Map[String, String])
         |    def getOrLeft(key: String): Either[String, String] =
         |      map.get(key) match
         |        case None        => Left(s"Missing ${key} in }")
         |        case Some(value) => Right(value)
         |""".stripMargin
    )

  @Test def `given-synthetic1` =
    check(
      """|given (usi@@ng i: Int): Double = 4.0
         |val a = given_Double""".stripMargin
    )

  @Test def `given-synthetic2` =
    check(
      """|given (using i: Int): Double = 4.0
         |val a = <<given_Doub@@le>>""".stripMargin
    )

  @Test def `given-synthetic3` =
    check(
      """|given Int = 10
         |val a = <<giv@@en_Int>>""".stripMargin
    )

  @Test def `given-synthetic4` =
    check(
      """|given <<I@@nt>> = 10
         |val a = given_Int""".stripMargin
    )

  @Test def `given-not-synthetic1` =
    check(
      """|given <<`giv@@en_D`>>: Double = 4.0
         |val a = <<`given_D`>>""".stripMargin
    )

  @Test def `given-not-synthetic2` =
    check(
      """|given <<`given_D`>>:Double = 4.0
         |val a = <<`giv@@en_D`>>""".stripMargin
    )

  @Test def `extension-with-type-param1` =
    check(
      """|extension [<<E@@F>>](xs: List[<<EF>>])
         |    def double(ys: List[<<EF>>]) = xs ++ ys
         |    def double2(ys: List[<<EF>>]) = xs ++ ys
         |end extension""".stripMargin
    )

  @Test def `extension-with-type-param2` =
    check(
      """|extension [EF, <<E@@M>>](xs: Either[<<EM>>, EF])
         |    def id() = xs
         |end extension""".stripMargin
    )

  @Test def `extension-with-type-param3` =
    check(
      """|extension [<<EF>>](xs: List[<<E@@F>>])
         |    def double(ys: List[<<EF>>]) = xs ++ ys
         |    def double2(ys: List[<<EF>>]) = xs ++ ys
         |end extension""".stripMargin
    )

  @Test def `extension-with-type-param4` =
    check(
      """|val i: <<Int>> = 3
         |extension (xs: List[<<In@@t>>])
         |  def id() = xs
         |end extension""".stripMargin
    )

  @Test def `extension-with-type-param5` =
    check(
      """|extension [<<EF>>](xs: List[<<EF>>])
         |    def double(ys: List[<<E@@F>>]) = xs ++ ys
         |    def double2(ys: List[<<EF>>]) = xs ++ ys
         |end extension""".stripMargin
    )

  @Test def `extension-with-type-param6` =
    check(
      """|extension [EF](xs: List[EF])
         |    def double(<<y@@s>>: List[EF]) = xs ++ <<ys>>
         |    def double2(ys: List[EF]) = xs ++ ys
         |end extension""".stripMargin
    )

  @Test def `extension-with-type-param7` =
    check(
      """|extension [EF](<<xs>>: List[EF])
         |    def double(ys: List[EF]) = <<x@@s>> ++ ys
         |    def double2(ys: List[EF]) = <<xs>> ++ ys
         |end extension""".stripMargin
    )

  @Test def `enum-cases` =
    check(
      """|enum MyOption:
         |  case <<My@@Some>>(value: Int)
         |  case MyNone
         |
         |val alpha = MyOption.<<MySome>>(1)
         |""".stripMargin
    )

  @Test def `enum-cases2` =
    check(
      """|enum MyOption:
         |  case <<My@@Some>>[U](value: U)
         |  case MyNone
         |
         |val alpha = MyOption.<<MySome>>(1)
         |""".stripMargin
    )

  @Test def `type-params-in-enum` =
    check(
      """|enum MyOption[+<<A@@A>>]:
         |  case MySome(value: <<AA>>)
         |  case MyNone
         |""".stripMargin
    )

  @Test def `type-params-in-enum2` =
    check(
      """|enum MyOption[+<<AA>>]:
         |  case MySome(value: <<A@@A>>)
         |  case MyNone
         |""".stripMargin
    )

  @Test def `type-params-in-enum3` =
    check(
      """|enum MyOption[<<AA>>](v: <<AA>>):
         |  def get: <<A@@A>> = ???
         |  case MySome[AA](value: AA) extends MyOption[Int](1)
         |""".stripMargin
    )

  @Test def `type-params-in-enum4` =
    check(
      """|enum MyOption[+<<AA>>]:
         |  def get: <<A@@A>> = ???
         |  case MySome(value: <<AA>>)
         |  case MyNone
         |""".stripMargin
    )

  @Test def `type-params-in-enum5` =
    check(
      """|enum MyOption[AA]:
         |  def get: AA = ???
         |  case MySome[<<AA>>](value: <<A@@A>>) extends MyOption[Int]
         |""".stripMargin
    )

  @Test def `implicit-extension` =
    check(
      """|class MyIntOut(val value: Int)
         |object MyIntOut:
         |  extension (i: MyIntOut) def <<uneven>> = i.value % 2 == 1
         |
         |val a = MyIntOut(1)
         |val m = a.<<un@@even>>
         |""".stripMargin
    )

  @Test def `implicit-extension-2` =
    check(
      """|class MyIntOut(val value: Int)
         |object MyIntOut:
         |  extension (i: MyIntOut) def <<uneven>>(u: Int) = i.value % 2 == 1
         |
         |val a = MyIntOut(1).<<un@@even>>(3)
         |""".stripMargin
    )

  @Test def `implicit-extension-infix` =
    check(
      """|class MyIntOut(val value: Int)
         |object MyIntOut:
         |  extension (i: MyIntOut) def <<++>>(u: Int) = i.value + u
         |
         |val a = MyIntOut(1) <<+@@+>> 3
         |""".stripMargin
    )

  @Test def `constructor` =
    check(
      """
        |object Main {
        |  class <<A@@bc>>[T](abc: T)
        |  val x = new <<Abc>>(123)
        |}""".stripMargin
    )

  @Test def `constructor1` =
    check(
      """
        |object Main {
        |  case class <<Abc>>[T](abc: T)
        |  val x = <<A@@bc>>(123)
        |}""".stripMargin
    )

  @Test def `constructor2` =
    check(
      """
        |object Main {
        |  class <<A@@bc>>[T](abc: T)
        |  object <<Abc>>
        |  val x = new <<Abc>>(123)
        |}""".stripMargin
    )

  @Test def `constructor3` =
    check(
      """
        |object Main {
        |  class <<Abc>>[T](abc: T)
        |  object <<Abc>>
        |  val x = new <<A@@bc>>(123)
        |}""".stripMargin
    )

  @Test def `constructor4` =
    check(
      """
        |object Main {
        |  class <<Abc>>[T](abc: T)
        |  object <<Ab@@c>>
        |  val x = new <<Abc>>(123)
        |}""".stripMargin
    )

  @Test def `constructor5` =
    check(
      """
        |object Main {
        |  class <<Abc>>[T](abc: T)
        |  object <<Abc>> {
        |    def apply(abc: Int, bde: Int) = new <<Abc>>(abc + bde)
        |  }
        |  val x = <<Ab@@c>>(123, 456)
        |}""".stripMargin
    )

  @Test def `constructor6` =
    check(
      """
        |class <<Abc>>[T](a: T)
        |object O {
        |  def foo(a: Int) = new <<Abc>>[Int](a)
        |  val x = <<Ab@@c>>[Int](2)
        |}""".stripMargin
    )

  @Test def `constructor7` =
    check(
      """
        |object Bar {
        |class <<Abc>>[T](a: T)
        |}
        |
        |object O {
        |  val x = new Bar.<<Ab@@c>>(2)
        |}""".stripMargin
    )

  @Test def `constructor8` =
    check(
      """
        |object Bar {
        |class <<Abc>>[T](a: T)
        |}
        |
        |object O {
        |  val x = Bar.<<Ab@@c>>[Int](2)
        |}""".stripMargin
    )

  @Test def `i5630` =
    check(
      """|class MyIntOut(val value: Int)
         |object MyIntOut:
         |  extension (i: MyIntOut) def <<uneven>> = i.value % 2 == 1
         |
         |val a = MyIntOut(1)
         |val m = a.<<un@@even>>
         |""".stripMargin
    )

  @Test def `i5630-2` =
    check(
      """|class MyIntOut(val value: Int)
          |object MyIntOut:
          |  extension (i: MyIntOut) def <<uneven>>(u: Int) = i.value % 2 == 1
          |
          |val a = MyIntOut(1).<<un@@even>>(3)
          |""".stripMargin
    )

  @Test def `i5630-infix` =
    check(
      """|class MyIntOut(val value: Int)
          |object MyIntOut:
          |  extension (i: MyIntOut) def <<++>>(u: Int) = i.value + u
          |
          |val a = MyIntOut(1) <<+@@+>> 3
          |""".stripMargin
    )

  @Test def `i5921-1` =
    check(
      """|object Logarithms:
          |  opaque type Logarithm = Double
          |  extension [K](vmap: Logarithm)
          |    def <<multiply>>(k: Logarithm): Logarithm = ???
          |
          |object Test:
          |  val in: Logarithms.Logarithm = ???
          |  in.<<multi@@ply>>(in)
          |""".stripMargin
    )

  @Test def `i5921-2` =
    check(
      """|object Logarithms:
          |  opaque type Logarithm = Double
          |  extension [K](vmap: Logarithm)
          |    def <<mu@@ltiply>>(k: Logarithm): Logarithm = ???
          |
          |object Test:
          |  val in: Logarithms.Logarithm = ???
          |  in.<<multiply>>(in)
          |""".stripMargin
    )

  @Test def `i5921-3` =
    check(
      """|object Logarithms:
          |  opaque type Logarithm = Double
          |  extension [K](vmap: Logarithm)
          |    def <<multiply>>(k: Logarithm): Logarithm = ???
          |  (2.0).<<mult@@iply>>(1.0)
          |""".stripMargin
    )

  @Test def `i5921-4` =
    check(
      """|object Logarithms:
          |  opaque type Logarithm = Double
          |  extension [K](vmap: Logarithm)
          |    def <<mult@@iply>>(k: Logarithm): Logarithm = ???
          |  (2.0).<<multiply>>(1.0)
          |""".stripMargin
    )

  @Test def `i5977` =
    check(
      """
        |sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def <<inferredTypeArg>>[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Int](_.<<infe@@rredTypeArg>>("str"))
        |  usage[Int](_.<<inferredTypeArg>>[String]("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>[String]("str"))
        |}
        |""".stripMargin
    )

  @Test def `i5977-1` =
    check(
      """
        |sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def <<inferredTypeArg>>[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Int](_.<<inferredTypeArg>>("str"))
        |  usage[Int](_.<<infe@@rredTypeArg>>[String]("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>[String]("str"))
        |}
        |""".stripMargin
    )

  @Test def `i5977-2` =
    check(
      """
        |sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def <<inferredTypeArg>>[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Int](_.<<inferredTypeArg>>("str"))
        |  usage[Int](_.<<inferredTypeArg>>[String]("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferr@@edTypeArg>>("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>[String]("str"))
        |}
        |""".stripMargin
    )

  @Test def `i5977-3` =
    check(
      """
        |sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def <<inferredTypeArg>>[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Int](_.<<inferredTypeArg>>("str"))
        |  usage[Int](_.<<inferredTypeArg>>[String]("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferred@@TypeArg>>[String]("str"))
        |}
        |""".stripMargin
    )

  @Test def `i5977-4` =
    check(
      """
        |sealed trait ExtensionProvider {
        |  extension [A] (self: A) {
        |    def typeArg[B <: A]: B
        |    def <<inferre@@dTypeArg>>[C](value: C): C
        |  }
        |}
        |
        |object Repro {
        |  def usage[A](f: ExtensionProvider ?=> A => Any): Any = ???
        |
        |  usage[Int](_.<<inferredTypeArg>>("str"))
        |  usage[Int](_.<<inferredTypeArg>>[String]("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>("str"))
        |  usage[Option[Int]](_.typeArg[Some[Int]].value.<<inferredTypeArg>>[String]("str"))
        |}
        |""".stripMargin
    )

  @Test def i3053 =
    check(
      s"""import Aaaa.*
        |
        |def classDef2(cdef: List[Int]): Int = {
        |  def aaa(ddef: Thicket2): List[Int] = ddef match {
        |    case Thicket2(_) => ???
        |  }
        |${("//" + "x" * 64 + "\n") * 64}
        |  1
        |}.<<showing2>>("aaa")
        |
        |case class Thicket2(trees: List[Int]) {}
        |
        |object Aaaa {
        |  extension [T](x: T)
        |    def <<show@@ing2>>[U](aaa: String): T = {
        |      x
        |    }
        |}
        |
        |""".stripMargin
    )

  @Test def i7256 =
    check(
      """|package a
         |object Test:
         |  def <<methodA>>: Unit = ???
         |export Test.<<me@@thodA>>
         |val i = <<methodA>>
         |""".stripMargin
    )

  @Test def `i7256-2` =
    check(
      """|object Test:
         |  def <<methodA>>: Unit = ???
         |  def methodB: Unit = ???
         |export Test.{<<me@@thodA>>, methodB}
         |val i = <<methodA>>
         |""".stripMargin
    )

  @Test def `i7256-3` =
    check(
      """|object Test:
         |  def <<methodA>>: Unit = ???
         |  def methodB: Unit = ???
         |export Test.<<methodA>>
         |val i = <<met@@hodA>>
         |val j = <<methodA>>
         |""".stripMargin
    )

  @Test def `i7256-4` =
    check(
      """|object Test:
         |  class <<Foo>>
         |  object <<Foo>>
         |export Test.<<F@@oo>>
         |""".stripMargin
    )

end DocumentHighlightSuite
