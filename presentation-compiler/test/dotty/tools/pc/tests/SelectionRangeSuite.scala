package dotty.tools.pc.tests

import dotty.tools.pc.base.BaseSelectionRangeSuite

import org.junit.Test

class SelectionRangeSuite extends BaseSelectionRangeSuite:

  @Test def `match` =
    check(
      """|object Main extends App {
         |  Option("chris") match {
         |    case Some(n@@ame) => println("Hello! " + name)
         |    case None =>
         |  }
         |}""".stripMargin,
      List(
        """|object Main extends App {
           |  Option("chris") match {
           |    case Some(>>region>>name<<region<<) => println("Hello! " + name)
           |    case None =>
           |  }
           |}""".stripMargin,
        """|object Main extends App {
           |  Option("chris") match {
           |    case >>region>>Some(name)<<region<< => println("Hello! " + name)
           |    case None =>
           |  }
           |}""".stripMargin,
        """|object Main extends App {
           |  Option("chris") match {
           |    >>region>>case Some(name) => println("Hello! " + name)<<region<<
           |    case None =>
           |  }
           |}""".stripMargin,
        """|object Main extends App {
           |  >>region>>Option("chris") match {
           |    case Some(name) => println("Hello! " + name)
           |    case None =>
           |  }<<region<<
           |}""".stripMargin,
        """|object Main extends >>region>>App {
           |  Option("chris") match {
           |    case Some(name) => println("Hello! " + name)
           |    case None =>
           |  }<<region<<
           |}""".stripMargin,
        """|>>region>>object Main extends App {
           |  Option("chris") match {
           |    case Some(name) => println("Hello! " + name)
           |    case None =>
           |  }
           |}<<region<<""".stripMargin
      )
    )

  @Test def `for` =
    check(
      """|object Main extends App {
         |  val total = for {
         |    a <- S@@ome(1)
         |    b <- Some(2)
         |  } yield a + b
         |}""".stripMargin,
      List(
        """|object Main extends App {
           |  val total = for {
           |    a <- >>region>>Some<<region<<(1)
           |    b <- Some(2)
           |  } yield a + b
           |}""".stripMargin,
        """|object Main extends App {
           |  val total = for {
           |    a <- >>region>>Some(1)<<region<<
           |    b <- Some(2)
           |  } yield a + b
           |}""".stripMargin,
        """|object Main extends App {
           |  val total = >>region>>for {
           |    a <- Some(1)
           |    b <- Some(2)
           |  } yield a + b<<region<<
           |}""".stripMargin,
        """|object Main extends App {
           |  >>region>>val total = for {
           |    a <- Some(1)
           |    b <- Some(2)
           |  } yield a + b<<region<<
           |}""".stripMargin,
        """|object Main extends >>region>>App {
           |  val total = for {
           |    a <- Some(1)
           |    b <- Some(2)
           |  } yield a + b<<region<<
           |}""".stripMargin,
        """|>>region>>object Main extends App {
           |  val total = for {
           |    a <- Some(1)
           |    b <- Some(2)
           |  } yield a + b
           |}<<region<<""".stripMargin
      )
    )

  @Test def `function params` =
    check(
      """|object Main extends App {
         |  def func(a@@: Int, b: Int) =
         |    a + b
         |}""".stripMargin,
      List[String](
        """|object Main extends App {
           |  def func(>>region>>a: Int<<region<<, b: Int) =
           |    a + b
           |}""".stripMargin,
        """|object Main extends App {
           |  def func(>>region>>a: Int, b: Int<<region<<) =
           |    a + b
           |}""".stripMargin,
        """|object Main extends App {
           |  >>region>>def func(a: Int, b: Int) =
           |    a + b<<region<<
           |}""".stripMargin
      )
    )
    check(
      """|object Main extends App {
         |  val func = (a@@: Int, b: Int) =>
         |    a + b
         |}""".stripMargin,
      List[String](
        """|object Main extends App {
           |  val func = (>>region>>a: Int<<region<<, b: Int) =>
           |    a + b
           |}""".stripMargin,
        """|object Main extends App {
           |  val func = (>>region>>a: Int, b: Int<<region<<) =>
           |    a + b
           |}""".stripMargin,
        """|object Main extends App {
           |  val func = >>region>>(a: Int, b: Int) =>
           |    a + b<<region<<
           |}""".stripMargin,
        """|object Main extends App {
           |  >>region>>val func = (a: Int, b: Int) =>
           |    a + b<<region<<
           |}""".stripMargin
      )
    )
