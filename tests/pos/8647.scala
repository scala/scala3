final class Two[A, B]()

final class Blaaa

final class Bla[X]

object Test1 {

  type Foo[X] = X match
    case Two[Blaaa, ?] =>
      String
    case Two[String, ?] =>
      Int

  def test: Foo[Two[String, String]] = 1
}

object Test2 {
  type Foo[X] = X match
    case Two[Bla[?], ?] =>
      String
    case Two[String, ?] =>
      Int

  def test: Foo[Two[String, String]] = 1
}


object Test3 {
  type Id[W] = W

  type M[X, Y] = X match {
    case Int   => String
    case Id[x] => Y match {
      case Two[Bla[a], ?] => Int
      case _ => String
    }
  }
  val x: M[Boolean, Two[Boolean, Boolean]] = ""
}

object Test4 {
  type Id[W] = W

  type M[X, Y] = X match {
    case Int   => String
    case Id[x] => Y match {
      case Two[Bla[`x`], ?] => Int
      case _ => String
    }
  }
  val x: M[Boolean, Two[Bla[Boolean], Boolean]] = 1
}
