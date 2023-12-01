//> using options -Werror
enum Color:
  case Red, Green

case class Wrapper[A](value: A)

object Test:
  def test_correct(x: Wrapper[Color]): Option[Wrapper[Color.Red.type]] = x match
    case x: Wrapper[Color.Red.type] => Some(x) // error
    case null                       => None

  def test_different(x: Wrapper[Color]): Option[Wrapper[Color]] = x match
    case x @ Wrapper(_: Color.Red.type)   => Some(x)
    case x @ Wrapper(_: Color.Green.type) => None

  def test_any(x: Any): Option[Wrapper[Color.Red.type]] = x match
    case x: Wrapper[Color.Red.type] => Some(x) // error
    case _                          => None

  def test_wrong(x: Wrapper[Color]): Option[Wrapper[Color.Red.type]] = x match
    case x: Wrapper[Color.Red.type] => Some(x) // error
    case null                       => None

  def t2[A1 <: Wrapper[Color]](x: A1): Option[Wrapper[Color.Red.type]] = x match
    case x: Wrapper[Color.Red.type] => Some(x) // error
    case null                       => None

  def test_wrong_seq(xs: Seq[Wrapper[Color]]): Seq[Wrapper[Color.Red.type]] =
    xs.collect {
      case x: Wrapper[Color.Red.type] => x // error
    }

  def test_wrong_seq2(xs: Seq[Wrapper[Color]]): Seq[Wrapper[Color.Red.type]] =
    xs.collect { x => x match
      case x: Wrapper[Color.Red.type] => x // error
    }

  def main(args: Array[String]): Unit =
    println(test_wrong_seq(Seq(Wrapper(Color.Red), Wrapper(Color.Green))))
    // outputs: List(Wrapper(Red), Wrapper(Green))
