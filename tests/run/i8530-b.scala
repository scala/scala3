import scala.compiletime.erasedValue

class MyRegex[Pattern <: String & Singleton/*Literal constant*/]:
  inline def unapplySeq(s: CharSequence): Option[List[String]] =
    inline erasedValue[Pattern] match
      case "foo" => if s == "foo" then Some(Nil) else None
      case _ => valueOf[Pattern].r.unapplySeq(s)

@main def Test: Unit =
  val myRegexp1 = new MyRegex["foo"]
  val myRegexp2 = new MyRegex["f(o+)"]
  "foo" match
    case myRegexp1() => // Match ok
    case myRegexp2(x) => ???
  "foooo" match
    case myRegexp1() => ???
    case myRegexp2(x) =>
      assert(x == "oooo")