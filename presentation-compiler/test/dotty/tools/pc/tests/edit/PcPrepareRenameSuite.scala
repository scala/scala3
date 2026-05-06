package dotty.tools.pc.tests.edit

import dotty.tools.pc.base.BasePcRenameSuite

import org.junit.Test

class PcPrepareRenameSuite extends BasePcRenameSuite:

  @Test def `prepare-import` =
    prepare(
      """|package a
         |
         |import java.util.{List => `J-List`}
         |
         |object Main{
         |  def m() = {
         |    val toRename: `J-L@@ist`[Int] = ???
         |    val toRename2: `J-List`[Int] = ???
         |    val toRename3: java.util.List[Int] = ???
         |  }
         |}
         |""".stripMargin
    )

  // currently we are not using presentation compiler in this case
  @Test def `prepare-import-object` =
    prepare(
      """|package a
         |
         |object Renaming {
         |  def m() = {
         |    import scala.util.{Try => StdLibTry}
         |    def foo(n: Int): StdLib@@Try[Int] =
         |      StdLibTryprepare(n)
         |  }
         |}
         |""".stripMargin
    )

  @Test def `case` =
    prepare(
      """|package a
         |case class Userprepare(name : String)
         |object Main{
         |  def m() = {
         |    val user = User.apply("James")
         |    val user2 = U@@serprepare(name = "Roger")
         |    user.copy(name = "")
         |  }
         |}
         |""".stripMargin
    )

  @Test def `generics` =
    prepare(
      """|package a
         |trait S1[X] { def torename(p: X): String = "" }
         |trait T1[Z] extends S1[Z] { override def tore@@name(p: Z): String = super.torename(p) }
         |""".stripMargin
    )

  @Test def `match-ret-type` =
    prepare(
      """|package a
         |trait P
         |trait PP extends P
         |trait A { def torename(a: String): P = ??? }
         |trait B extends A { override def tore@@name(a: String): PP = ??? }
         |
         |""".stripMargin
    )

  @Test def `unapply` =
    prepare(
      """|object F@@oo {
         |  def unapply(s: String): Option[String] = Some("")
         |}
         |
         |object Main{
         |  def m() = {
         |    "foo" match {
         |      case Fooprepare(s) => ()
         |    }
         |  }
         |}
         |""".stripMargin
    )

  @Test def `unapply-param` =
    prepare(
      """|object Foo {
         |  def unapply(<<nam@@e>>: String): Option[String] = Some(name)
         |}
         |
         |object Main{
         |  "foo" match {
         |    case Foo(name) => ()
         |  }
         |}
         |""".stripMargin
    )

  @Test def `local` =
    prepare(
      """|package a
         |object Main{
         |  def hello() = {
         |    val <<toRen@@ame>> = 123
         |    toRename
         |  }
         |}
         |""".stripMargin
    )

  @Test def `method` =
    prepare(
      """|package a
         |object Main{
         |  def m() = {
         |    def <<met@@hodprepare>>(abc : String) = true
         |    if(methodprepare("")) println("Is true!")
         |  }
         |}
         |""".stripMargin
    )

  @Test def `self-type` =
    prepare(
      """|package a
         |
         |object Main{
         |  def m() = {
         |    trait <<A@@BC>>
         |    trait Alphabet{
         |      this: ABC =>
         |    }
         |    val a = new Alphabet with ABC
         |  }
         |}
         |""".stripMargin
    )

  @Test def `method-inheritance` =
    prepare(
      """|package a
         |trait Hello{
         |  def method(abc : String) : Boolean
         |}
         |
         |class GoodMorning extends Hello {
         |  def met@@hod(abc : String) = true
         |}
         |""".stripMargin
    )

  @Test def `apply` =
    prepare(
      """|package a
         |object User{
         |  def ap@@ply(name : String) = name
         |  def apply(name : String, age: Int) = name
         |}
         |object Main{
         |  val toRename = User##.##("abc")
         |}
         |""".stripMargin
    )

  @Test def `colon-bad` =
    prepare(
      """|package a
         |object Main{
         |  def m() = {
         |    class User{
         |      def <<:@@:>>(name : String) = name
         |    }
         |    val user = new User()
         |    "" :: user
         |  }
         |}
         |""".stripMargin
    )

  @Test def `unary-bad` =
    prepare(
      """|package a
         |
         |object Main{
         |  def m() = {
         |    class User{
         |      def unary_! = false
         |    }
         |    val user = new User()
         |    @@!user
         |  }
         |}
         |""".stripMargin
    )

  @Test def `java-classes` =
    prepare(
      """|package a
         |class MyException extends Exce@@ption
         |class NewException extends RuntimeException
         |class NewException2 extends RuntimeException
         |""".stripMargin
    )

  @Test def `companion` =
    prepare(
      """|package a
         |class Main{}
         |object M@@ain
         |""".stripMargin
    )

  @Test def `companion2` =
    prepare(
      """|package a
         |object a {
         |  def m() = {
         |    class <<Ma@@in>>{}
         |    object Main
         |
         |  }
         |}
         |""".stripMargin
    )

  @Test def `anon` =
    prepare(
      """|trait Methodable[T] {
         |  def methodprepare(asf: T): Int
         |}
         |
         |trait Alphabet extends Methodable[String] {
         |  def methodprepare(adf: String) = 123
         |}
         |
         |object Main {
         |  val a = new Alphabet {
         |    override def me@@thod(adf: String): Int = 321
         |  }
         |}
         |""".stripMargin
    )

  @Test def `anon2` =
    prepare(
      """|object a {
         |  def m() = {
         |    trait Methodable[T] {
         |      def methodprepare(asf: T): Int
         |    }
         |
         |    trait Alphabet extends Methodable[String] {
         |      def methodprepare(adf: String) = 123
         |    }
         |
         |    object Main {
         |      val a = new Alphabet {
         |        override def <<me@@thod>>(adf: String): Int = 321
         |    }
         |  }
         |}
         |""".stripMargin
    )

  @Test def `macro` =
    prepare(
      """|package a
         |import io.circe.generic.JsonCodec
         |trait LivingBeing
         |object Main {
         |  def m() = {
         |    @JsonCodec sealed trait <<An@@imal>> extends LivingBeing
         |    object Animal {
         |      case object Dog extends Animal
         |      case object Cat extends Animal
         |    }
         |  }
         |}
         |""".stripMargin
    )

  @Test def `macro2` =
    prepare(
      """|package a
         |import io.circe.generic.JsonCodec
         |@JsonCodec
         |final case class Ma@@in2(name: String)
         |""".stripMargin
    )

  @Test def `implicit-param` =
    prepare(
      """|package a
         |object A {
         |  implicit val some@@Name: Int = 1
         |  def m[A](implicit a: A): A = a
         |  m[Int]
         |}""".stripMargin
    )

  @Test def `backtick2` =
    prepare(
      """|package a
         |object Main{
         |  val greeting = "Hello"
         |  "" match {
         |    case `gre@@eting` =>
         |  }
         |}
         |""".stripMargin
    )

  @Test def `backtick3` =
    prepare(
      """|package a
         |object Main{
         |  def local = {
         |    val <<greet@@ing>> = "Hello"
         |    "" match {
         |      case `greeting` =>
         |    }
         |  }
         |}
         |""".stripMargin
    )

  // If renaming in VS Code, backticks are taken as part of the name
  @Test def `backtick4` =
    prepare(
      """|package a
         |object Main{
         |  def local = {
         |    val greeting = "Hello"
         |    "" match {
         |      case <<`gre@@eting`>> =>
         |    }
         |  }
         |}
         |""".stripMargin
    )

  @Test def `params` =
    prepare(
      """|
         |
         |object Main {
         |  def m() = {
         |    case class Name(<<va@@lue>>: String)
         |    val name1 = Name(value = "42")
         |      .copy(value = "43")
         |      .copy(value = "43")
         |      .value
         |    val name2 = Name(value = "44")
         |  }
         |}
         |""".stripMargin
    )

  @Test def `constructor` =
    prepare(
      """|case class Name(va@@lue: String)
         |
         |object Main {
         |  val name2 = new Name(value = "44")
         |}
         |""".stripMargin
    )

  @Test def `type-params` =
    prepare(
      """|package a
         |trait ABC
         |class CBD[T <: AB@@C]
         |object Main{
         |  val a = classOf[ABC]
         |  val b = new CBD[ABC]
         |}
         |""".stripMargin
    )

  @Test def `hierarchy-inside-method-trait` =
    prepare(
      """|package a
         |object Main {
         |  final def main(args: Array[String]) = {
         |    sealed trait <<Sy@@mbol>>
         |    case class Method(name: String) extends Symbol
         |    case class Variable(value: String) extends Symbol
         |
         |    val symbol2: Symbol = Method("method")
         |    val symbol3: Symbol = Variable("value")
         |  }
         |}
         |""".stripMargin
    )

  @Test def `hierarchy-inside-method-class` =
    prepare(
      """|package a
         |object Main {
         |  final def main(args: Array[String]) = {
         |    sealed abstract class <<Sy@@mbol>>
         |    case class Method(name: String) extends Symbol
         |    case class Variable(value: String) extends Symbol
         |
         |    val symbol2: Symbol = Method("method")
         |    val symbol3: Symbol = Variable("value")
         |  }
         |}
         |""".stripMargin
    )

  @Test def `variable` =
    prepare(
      """|package a
         |object Main {
         |  var v@@5 = false
         |
         |  def f5: Boolean = {
         |    v5 = true
         |    v5 == true
         |  }
         |}
         |""".stripMargin
    )

  @Test def `variable-explicit1` =
    prepare(
      """|package a
         |object Main {
         |  var v@@5 = false
         |
         |  def f5: Boolean = {
         |    v5_=(true)
         |    v5 == true
         |  }
         |}
         |""".stripMargin
    )

  @Test def `worksheet-method` =
    prepare(
      """|trait S1[X] { def torename(p: X): String = "" }
         |trait T1[Z] extends S1[Z] { override def <<tore@@name>>(p: Z): String = super.torename(p) }
         |""".stripMargin,
      filename = "A.worksheet.sc"
    )

  @Test def `worksheet-classes` =
    prepare(
      """|sealed abstract class <<Sy@@mbol>>
         |case class Method(name: String) extends Symbol
         |case class Variable(value: String) extends Symbol
         |""".stripMargin,
      filename = "A.worksheet.sc"
    )

  @Test def `not-compiling` =
    prepare(
      """|package a
         |object Main {
         |  def method() = {
         |    List(1) + 2
         |    val abc: Option[Int] = ???
         |    <<ab@@c>>.map(_ + 1)
         |  }
         |}
         |""".stripMargin
    )

  @Test def `extension-param` =
    prepare(
      """|extension (<<sb@@d>>: String)
         |  def double = sbd + sbd
         |  def double2 = sbd + sbd
         |end extension
         |""".stripMargin
    )

  @Test def `extension-params-ref` =
    prepare(
      """|extension (sbd: String)
         |  def double = <<sb@@d>> + sbd
         |  def double2 = sbd + sbd
         |end extension
         |""".stripMargin
    )

  @Test def `extension-type-param` =
    prepare(
      """|extension [T](<<x@@s>>: List[T])
         |  def double = xs ++ xs
         |  def double2 = xs ++ xs
         |end extension
         |""".stripMargin
    )

  @Test def `extension-type-param-ref` =
    prepare(
      """|extension [T](xs: List[T])
         |  def double = xs ++ xs
         |  def double2 = xs ++ <<x@@s>>
         |end extension
         |""".stripMargin
    )

  @Test def `named-arg-backtick` =
    prepare(
      """|object Main {
         |  def m() = {
         |    def foo(`type`: String): String = `type`
         |    val x = foo(
         |      <<`ty@@pe`>> = "abc"
         |    )
         |  }
         |}
         |""".stripMargin
    )
