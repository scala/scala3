//> using options -Yexplicit-nulls
import language.experimental.magic
import scala.compiletime.erasedValue

class MyRegex[Pattern <: String & Singleton/*Literal constant*/]:
  inline def unapplySeq(s: CharSequence): List[String]? =
    inline erasedValue[Pattern] match
      case "foo" => if s == "foo" then Nil else null
      case _ => valueOf[Pattern].r.unapplySeq(s) match
        case Some(xs) => xs // error
        case None => null
