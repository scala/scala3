package dotty.tools.pc.tests.signaturehelp

import dotty.tools.pc.base.BaseSignatureHelpSuite

import org.junit.Ignore
import org.junit.Test

class SignatureHelpPatternSuite extends BaseSignatureHelpSuite:

  @Ignore
  @Test def `case` =
    check(
      """
        |object Main {
        |  List(1 -> 2).map {
        |    case (a, @@) =>
        |  }
        |}
        |""".stripMargin,
      """|(Int, Int)
         |      ^^^
         |""".stripMargin
    )

  @Test def `generic1` =
    check(
      """
        |object Main {
        |  Option(1) match {
        |    case Some(@@) =>
        |  }
        |}
        |""".stripMargin,
      // This test is changed because we're using bootstrapped stdlib
      // and in Scala3 the return type of unapply is Some[Int] which
      // satisfies the predicate for param name ( is case class and is synthetic )
      """|(Int)
         | ^^^
         |""".stripMargin
    )

  @Test def `generic2` =
    check(
      """
        |case class Two[T](a: T, b: T)
        |object Main {
        |  (null: Any) match {
        |    case Two(@@) =>
        |  }
        |}
        |""".stripMargin,
      """|(a: T, b: T)
         | ^^^^
         |""".stripMargin
    )

  @Test def `generic3` =
    check(
      """
        |case class HKT[C[_], T](a: C[T])
        |object Main {
        |  (null: Any) match {
        |    case HKT(@@) =>
        |  }
        |}
        |""".stripMargin,
      """|(a: C[T])
         | ^^^^^^^
         |""".stripMargin
    )

  @Test def `generic4` =
    check(
      """
        |  case class Two[A, B](a: A, b: B)
        |  object Main {
        |    new Two(1, "") match {
        |      case Two(@@) =>
        |    }
        |  }
        |""".stripMargin,
      """|(a: Int, b: String)
         | ^^^^^^
         |""".stripMargin
    )

  @Test def `generic5` =
    check(
      """
        |class Two[A, B](a: A, b: B)
        |object Two {
        |  def unapply[A, B](t: Two[A, B]): Option[(A, B)] = None
        |}
        |
        |object Main {
        |  val tp = new Two(1, "")
        |  tp match {
        |    case Two(@@) =>
        |  }
        |}
        |""".stripMargin,
      """|(Int, String)
         | ^^^
         |""".stripMargin
    )

  @Test def `non-synthetic-unapply` =
    check(
      """
        |class HKT[C[_], T](a: C[T])
        |object HKT {
        |  def unapply(a: Int): Option[(Int, Int)] = Some(2 -> 2)
        |}
        |object Main {
        |  (null: Any) match {
        |    case HKT(@@) =>
        |  }
        |}
        |""".stripMargin,
      """|(Int, Int)
         | ^^^
         |""".stripMargin
    )

  @Test def `non-synthetic-unapply-second` =
    check(
      """
        |class HKT[C[_], T](a: C[T])
        |object HKT {
        |  def unapply(a: Int): Option[(Int, Int)] = Some(2 -> 2)
        |}
        |object Main {
        |  (null: Any) match {
        |    case HKT(1, @@) =>
        |  }
        |}
        |""".stripMargin,
      """|(Int, Int)
         |      ^^^
         |""".stripMargin
    )

  @Test def `pat` =
    check(
      """
        |case class Person(name: String, age: Int)
        |object a {
        |  null.asInstanceOf[Person] match {
        |    case Person(@@)
        |}
      """.stripMargin,
      """|(name: String, age: Int)
         | ^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `pat1` =
    check(
      """
        |class Person(name: String, age: Int)
        |object Person {
        |  def unapply(p: Person): Option[(String, Int)] = ???
        |}
        |object a {
        |  null.asInstanceOf[Person] match {
        |    case Person(@@) =>
        |  }
        |}
      """.stripMargin,
      """|(String, Int)
         | ^^^^^^
         | """.stripMargin
    )

  @Ignore
  @Test def `pat2` =
    check(
      """
        |object a {
        |  val Number = "$a, $b".r
        |  "" match {
        |    case Number(@@)
        |  }
        |}
      """.stripMargin,
      """|(List[String])
         | ^^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `pat3` =
    check(
      """
        |object And {
        |  def unapply[A](a: A): Some[(A, A)] = Some((a, a))
        |}
        |object a {
        |  "" match {
        |    case And("", s@@)
        |  }
        |}
        |""".stripMargin,
      """|(String, String)
         |         ^^^^^^
         |""".stripMargin
    )

  @Test def `pat5` =
    check(
      """
        |object OpenBrowserCommand {
        |  def unapply(command: String): Option[Int] = {
        |    Some(1)
        |  }
        |
        |  "" match {
        |    case OpenBrowserCommand(@@) =>
        |  }
        |}
      """.stripMargin,
      """|(Int)
         | ^^^
         |""".stripMargin
    )

  @Test def `pat6` =
    check(
      """
        |object OpenBrowserCommand {
        |  def unapply(command: String): Option[Option[Int]] = {
        |    Some(Some(1))
        |  }
        |
        |  "" match {
        |    case OpenBrowserCommand(@@) =>
        |  }
        |}
      """.stripMargin,
      """|(Option[Int])
         | ^^^^^^^^^^^
         |""".stripMargin
    )

  @Test def `shortened` =
    check(
      """
        |object Test {
        |  def unapply(command: java.io.File): Option[java.io.File] = {
        |    Some(Some(1))
        |  }
        |
        |  "" match {
        |    case Test(@@) =>
        |  }
        |}
      """.stripMargin,
      """|(File)
         | ^^^^
         |""".stripMargin
    )

  @Test def `pat-negative` =
    check(
      """
        |object And {
        |  def unapply[A](a: A): Some[(A, A)] = Some((a, a))
        |}
        |object a {
        |  And.unapply(@@)
        |}
      """.stripMargin,
      """|unapply[A](a: A): Some[(A, A)]
         |           ^^^^
         | """.stripMargin
    )
