package dotty.tools.pc.tests.definition

import scala.jdk.CollectionConverters.*
import scala.language.unsafeNulls
import scala.meta.pc.OffsetParams

import dotty.tools.pc.base.BasePcDefinitionSuite
import dotty.tools.pc.utils.MockEntries

import org.eclipse.lsp4j.Location
import org.junit.Ignore
import org.junit.Test

class TypeDefinitionSuite extends BasePcDefinitionSuite:

  override def mockEntries = new MockEntries:
    override def definitions = Map[String, List[Location]](
      MockLocation("scala/Option#", "Option.scala"),
      MockLocation("scala/Unit#", "Unit.scala"),
      MockLocation("scala/List#", "Unit.scala"),
      MockLocation("scala/Boolean#", "Boolean.scala"),
      MockLocation("scala/collection/WithFilter#", "WithFilter.scala"),
      MockLocation("scala/Option#WithFilter#", "Option.scala"),
      MockLocation("scala/collection/immutable/List#", "List.scala"),
      MockLocation("scala/Predef.String#", "Predef.scala"),
      MockLocation("java/lang/String#", "String.java"),
      MockLocation("scala/Int#", "Int.scala"),
      MockLocation("scala/concurrent/Future#", "Future.scala"),
      MockLocation("scala/concurrent/Future.", "Future.scala")
    )

  override def definitions(offsetParams: OffsetParams): List[Location] =
    presentationCompiler
      .typeDefinition(offsetParams)
      .get()
      .locations
      .asScala
      .toList

  @Test def `val` =
    check(
      """|class <<TClass>>(i: Int)
         |
         |object Main {
         |  val ts@@t = new TClass(2)
         |}""".stripMargin
    )

  @Test def `for` =
    check(
      """|
         |object Main {
         |  for {
         |    x <- List(1)
         |    y <- 1.to(x)
         |    z = y + x
         |    if y < /*scala/Int# Int.scala*/@@x
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `for-flatMap` =
    check(
      """|
         |object Main {
         |  for {
         |    x /*scala/Option# Option.scala*/@@<- Option(1)
         |    y <- Option(x)
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `for-map` =
    check(
      """|
         |object Main {
         |  for {
         |    x <- Option(1)
         |    y /*scala/Option# Option.scala*/@@<- Option(x)
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `for-withFilter` =
    check(
      """|
         |object Main {
         |  for {
         |    x <- Option(1)
         |    y <- Option(x)
         |    /*scala/Option#WithFilter# Option.scala*/@@if y > 2
         |  } yield y
         |}
         |""".stripMargin
    )

  @Test def `constructor` =
    check(
      """
        |class <<TClass>>(i: Int) {}
        |
        |object Main {
        | def tst(m: TClass): Unit = {}
        |
        |  tst(new T@@Class(2))
        |}""".stripMargin
    )

  @Test def `function` =
    check(
      """|
         |object Main {
         |  val increment: Int => Int = _ + 2
         |  incre/*scala/Int# Int.scala*/@@ment(1)
         |}
         |""".stripMargin
    )

  @Test def `tuple` =
    // assert we don't go to `Tuple2.scala`
    check(
      """|
         |object Main {
         |  @@(1, 2)
         |}
         |""".stripMargin
    )

  @Test def `method` =
    check(
      """|object Main {
         |  def tst(): Unit = {}
         |
         |  ts@@/*scala/Unit# Unit.scala*/t()
         |}""".stripMargin
    )

  @Test def `named-arg-multiple` =
    check(
      """|object Main {
         |  def tst(par1: Int, par2: String, par3: Boolean): Unit = {}
         |
         |  tst(1, p/*scala/Boolean# Boolean.scala*/@@ar3 = true, par2 = "")
         |}""".stripMargin
    )

  @Test def `named-arg-reversed` =
    check(
      """|object Main {
         |  def tst(par1: Int, par2: String): Unit = {}
         |
         |  tst(p/*scala/Predef.String# Predef.scala*/@@ar2 = "foo", par1 = 1)
         |}""".stripMargin
    )

  @Test def `named-arg-local` =
    check(
      """|object Main {
         |  def foo(arg: Int): Unit = ()
         |
         |  foo(a/*scala/Int# Int.scala*/@@rg = 42)
         |}
         |""".stripMargin
    )

  @Test def `named-arg-global` =
    check(
      """|object Main {
         |  assert(a/*scala/Boolean# Boolean.scala*/@@ssertion = true)
         |}
         |""".stripMargin
    )

  @Test def `list` =
    check(
      """|object Main {
         |  List(1).hea/*scala/Int# Int.scala*/@@d
         |}
         |""".stripMargin
    )

  @Test def `class` =
    check(
      """|object Main {
         |  class <<F@@oo>>(val x: Int)
         |}
         |""".stripMargin
    )

  @Test def `val-keyword` =
    check(
      """|object Main {
         |  va@@l x = 42
         |}
         |""".stripMargin
    )

  @Test def `literal` =
    check(
      """|object Main {
         |  val x = 4/*scala/Int# Int.scala*/@@2
         |}
         |""".stripMargin
    )

  @Test def `if` =
    check(
      """|object Main {
         |  for {
         |    x <- List(1)
         |    i/*scala/collection/WithFilter# WithFilter.scala*/@@f x > 1
         |  } println(x)
         |}
         |""".stripMargin
    )

  @Ignore
  @Test def `string` =
    check(
      """|object Main {
         |  "".stripS/*java/lang/String# String.java*/@@uffix("foo")
         |}
         |""".stripMargin
    )

  @Test def `method-generic` =
    check(
      """|object Main {
         |  def foo[<<T>>](param: T): T = para@@m
         |}
         |""".stripMargin
    )

  @Test def `method-generic-result` =
    check(
      """|object A {
         |  def foo[T](param: T): T = param
         |}
         |object Main {
         |  println(A.fo/*scala/Int# Int.scala*/@@o(2))
         |}
         |""".stripMargin
    )

  @Test def `apply` =
    check(
      """|
         |object Main {
         |  /*scala/collection/immutable/List# List.scala*/@@List(1)
         |}
         |""".stripMargin
    )

  @Test def `new` =
    check(
      """|
         |object Main {
         |  ne@@w java.io.File("")
         |}
         |""".stripMargin
    )

  @Test def `extends` =
    check(
      """|
         |object Main ex@@tends java.io.Serializable {
         |}
         |""".stripMargin
    )

  @Test def `import1` =
    check(
      """|
         |import scala.concurrent./*scala/concurrent/Future# Future.scala*//*scala/concurrent/Future. Future.scala*/@@Future
         |object Main {
         |}
         |""".stripMargin
    )

  @Test def `import2` =
    check(
      """|
         |imp@@ort scala.concurrent.Future
         |object Main {
         |}
         |""".stripMargin
    )

  @Test def `import3` =
    check(
      """|
         |import scala.co@@ncurrent.Future
         |object Main {
         |}
         |""".stripMargin
    )

  @Test def `symbolic-infix` =
    check(
      """|
         |object Main {
         |  val lst = 1 /*scala/collection/immutable/List# List.scala*/@@:: Nil
         |}
         |""".stripMargin
    )

  @Test def `package` =
    check(
      """|
         |object Main {
         |  val n = ma@@th.max(1, 2)
         |}
         |""".stripMargin
    )

  @Test def `eta` =
    check(
      """|
         |object Main {
         |  List(1).map(/*scala/Int# Int.scala*/@@_ + 2)
         |}
         |""".stripMargin
    )

  @Test def `eta-2` =
    check(
      """|
         |object Main {
         |  List(1).foldLeft(0)(_ + /*scala/Int# Int.scala*/@@_)
         |}
         |""".stripMargin
    )

  @Test def `result-type` =
    check(
      """|
         |object Main {
         |  def x: /*scala/Int# Int.scala*/@@Int = 42
         |}
         |""".stripMargin
    )

  @Test def `do-not-point-at ::` =
    check(
      """|
         |class Main {
         |  val all = Option(42)./*scala/Int# Int.scala*/@@get :: List("1", "2")
         |}
         |""".stripMargin
    )

  @Test def `synthetic-definition-case-class` =
    check(
      """|
         |class Main {
         |  case class <<User>>(name: String, age: Int)
         |  def hello(u: User): Unit = ()
         |  hello(Us@@er())
         |}
         |""".stripMargin
    )

  @Test def `synthetic-definition-class-constructor` =
    check(
      """|
         |class Main {
         |  class <<User>>(name: String, age: Int)
         |  def hello(u: User): Unit = ()
         |  hello(new Us@@er())
         |}
         |""".stripMargin
    )
