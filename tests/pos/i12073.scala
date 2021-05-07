inline def ff: Unit =
  inline 1 match
    case 1 | 2 =>

def test = ff

transparent inline def f: Option[String] =
  None

object Override {

  trait Trait { def s: String }

  object OK extends Trait {
    override transparent inline def s: String =
      inline f match {
        case Some("x") => "x"
        case Some("y") => "y"
        case None      => "-"
      }
  }

  object KO_1 extends Trait {
    override transparent inline def s: String =
      inline f match {
        case Some("x") => "x"
        case Some("y")
           | None      => "0"
      }
  }

  object KO_2 extends Trait {
    override transparent inline def s: String =
      inline f match {
        case Some("x") => "x"
        case Some("y") => "y"
        case Some(z)   => "z"
        case None      => "0"
      }
  }
}

object NonOverride {

  transparent inline def ok_1: String =
    inline f match {
      case Some("x") => "x"
      case Some("y") => "y"
      case None      => "-"
    }

  // ok: Some("y") | None
  transparent inline def ok_2: String =
    inline f match {
      case Some("x") => "x"
      case Some("y")
         | None      => "0"
    }

  // ok: no None
  transparent inline def ok_3: String =
    inline f match {
      case Some("x") => "x"
      case Some("y") => "y"
      case Some(z)   => "z"
      case None      => "0"
    }

  ok_1 + ok_2 ++ ok_3
}