package dotty.tools.pc.tests.completion

import dotty.tools.pc.base.BaseCompletionSuite

import org.junit.FixMethodOrder
import org.junit.Test
import org.junit.runners.MethodSorters

@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class CompletionNamedPatternSuite extends BaseCompletionSuite:

  @Test def `named-tuples-1` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |type User = (id: Int, name: String, surname: String)
         |val user = (id = 5, name = "Bob", surname = "Marley")
         |
         |def idsWithName(name: String) = user match
         |  case (nam@@
         |""".stripMargin,
      """name = : String
        |surname = : String""".stripMargin
    )

  @Test def `named-tuples-1-without-parens` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |type User = (id: Int, name: String, surname: String)
         |val user = (id = 5, name = "Bob", surname = "Marley")
         |
         |def idsWithName(name: String) = user match
         |  case nam@@
         |""".stripMargin,
      ""
    )


  @Test def `named-tuples-2` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |type User = (id: Int, name: String, surname: String)
         |val user = (id = 5, name = "Bob", surname = "Marley")
         |
         |def idsWithName(name: String) = user match
         |  case (name = supername, na@@
         |""".stripMargin,
      """surname = : String""".stripMargin
    )

  @Test def `named-tuples-3` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |type User = (id: Int, name: String, surname: String)
         |val user = (id = 5, name = "Bob", surname = "Marley")
         |
         |def idsWithName(name: String) = user match
         |  case (na@@, name = name) =>
         |""".stripMargin,
      """surname = : String""".stripMargin
    )

  @Test def `named-tuples-synthetic` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User(id: Int, name: String, surname: String)
         |
         |extension (values: Seq[User])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(name = `name`, id = userId) => userId
         |    case User(nam@@
         |""".stripMargin,
      """name = : String
        |surname = : String""".stripMargin
    )

  @Test def `named-tuples-custom-extractor` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |object MegaUser:
         |  def unapply(x: User): Option[(firstName: String, surname: String)] = ???
         |case class User(id: Int, name: String, surname: String)
         |
         |extension (values: Seq[User])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(name = `name`, id = userId) => userId
         |    case MegaUser(nam@@
         |""".stripMargin,
      """firstName = : String
        |surname = : String""".stripMargin
    )

  @Test def `named-tuples-implicit-synthetic` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User(id: Int, name: String, surname: String)(using String)
         |
         |extension (values: Seq[User])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(name = `name`, id = userId) => userId
         |    case User(nam@@
         |""".stripMargin,
      """name = : String
        |surname = : String""".stripMargin
    )

  @Test def `named-tuples-type-param-synthetic` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User[T](id: Int, name: String, surname: String, name2: T)
         |
         |extension (values: Seq[User[_]])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(name = `name`, id = userId) => userId
         |    case User(nam@@
         |""".stripMargin,
      """name = : String
        |name2 = : T
        |surname = : String""".stripMargin
    )

  @Test def `named-tuples-type-param-synthetic-concrete` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User[T](id: Int, name: String, surname: String, name2: T)
         |
         |extension (values: Seq[User[Int]])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(name = `name`, id = userId) => userId
         |    case User(nam@@
         |""".stripMargin,
      """name = : String
        |name2 = : Int
        |surname = : String""".stripMargin
    )

  @Test def `named-tuples-bind-with-named-patterns-1` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User(id: Int, name: String, surname: String)
         |
         |extension (values: Seq[User])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(surname = test, nam@@
         |""".stripMargin,
      "name = : String"
    )

  @Test def `named-tuples-bind-with-named-patterns-2` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User(id: Int, name: String, surname: String)
         |
         |extension (values: Seq[User])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(surname = test, nam@@) =>
         |""".stripMargin,
      "name = : String"
    )

  // TODO completion for binds but only in backtics
  // @Test def `named-tuples-bind-with-named-patterns-3` =
  //   check(
  //     """
  //        |import scala.language.experimental.namedTuples
  //        |
  //        |case class User(id: Int, name: String, surname: String)
  //        |val veryGoodName = "Bob"
  //        |
  //        |extension (values: Seq[User])
  //        |  //  Collect user IDs of every entry that has the name matching argument
  //        |  def idsWithName(name: String) = values.collect:
  //        |    case User(surname = test, name = `ver@@
  //        |""".stripMargin,
  //     "name = : String"
  //   )

  @Test def `nested-unapply-in-named-tuples-1` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |type Person = (name: Option[String], age: Int)
         |
         |val person = (name = Some("Bob"), age = 33)
         |def test = person match
         |  case (name = Som@@
         |
         |""".stripMargin,
      """Some(value) scala
        |Some scala""".stripMargin
    )

  @Test def `nested-unapply-in-named-tuples-2` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |type Person = (name: Option[String], age: Int)
         |type AnotherPerson = (name: Option[Person], age: Int)
         |
         |val person: Person = (name = Some("Bob"), age = 33)
         |val anotherPerson = (name = Some(person), age = 33)
         |
         |def test = anotherPerson match
         |  case (name = Some((na@@
         |
         |""".stripMargin,
      "name = : Option[String]"
    )

  @Test def `nested-unapply-in-named-tuples-3` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |case class Person(name: Option[String], age: Int)
         |
         |val person = Person(name = Some("Bob"), age = 33)
         |def test = person match
         |  case (name = Som@@
         |
         |""".stripMargin,
      """Some(value) scala
        |Some scala
        |Some scala""".stripMargin
    )

  @Test def `nested-unapply-in-named-tuples-4` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |case class Person(name: Option[String], age: Int)
         |case class AnotherPerson(name: Option[Person], age: Int)
         |
         |val person = Person(name = Some("Bob"), age = 33)
         |val anotherPerson = AnotherPerson(name = Some(person), age = 33)
         |
         |def test = anotherPerson match
         |  case Person(name = Some(AnotherPers@@
         |
         |""".stripMargin,
      "AnotherPerson test"
      //FIXME There should also be unapply for AnotherPerson(name: Option[Person], age: Int) but this is not a bug
      // introduced in this commit
    )

  // TODO Leaving this test as I want to make it work in the future, same for named args
  // @Test def `named-tuples-bind-with-named-patterns-2` =
  //   check(
  //     """
  //        |import scala.language.experimental.namedTuples
  //        |
  //        |case class User(id: Int, name: String, surname: String)
  //        |
  //        |extension (values: Seq[User])
  //        |  //  Collect user IDs of every entry that has the name matching argument
  //        |  def idsWithName(name: String) = values.collect:
  //        |    case User(nam@@ surname = test) =>
  //        |""".stripMargin,
  //     "name = : String"
  //   )

  @Test def `named-tuples-bind-with-named-patterns-4` =
    check(
      """
         |import scala.language.experimental.namedTuples
         |
         |case class User(id: Int, name: String, surname: String)
         |
         |extension (values: Seq[User])
         |  //  Collect user IDs of every entry that has the name matching argument
         |  def idsWithName(name: String) = values.collect:
         |    case User(nam@@, surname = test) =>
         |""".stripMargin,
      "name = : String"
    )
