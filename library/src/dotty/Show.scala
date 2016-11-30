package dotty

import scala.annotation.implicitNotFound

@implicitNotFound("No member of type class Show could be found for ${T}")
trait Show[-T] {
  def show(t: T): String
}

object Show {
  private[this] val defaultShow = new Show[Any] {
    def show(x: Any) = x.toString
  }

  implicit class ShowValue[V](val v: V) extends AnyVal {
    def show(implicit ev: Show[V] = defaultShow): String =
      ev.show(v)
  }

  implicit val stringShow = new Show[String] {
    // From 2.12 spec:
    //
    // charEscapeSeq ::= ‘\‘ (‘b‘ | ‘t‘ | ‘n‘ | ‘f‘ | ‘r‘ | ‘"‘ | ‘'‘ | ‘\‘)
    def show(str: String) =
      "\"" + {
        val sb = new StringBuilder
        str.foreach {
          case '\b' => sb.append("\\b")
          case '\t' => sb.append("\\t")
          case '\n' => sb.append("\\n")
          case '\f' => sb.append("\\f")
          case '\r' => sb.append("\\r")
          case '\'' => sb.append("\\'")
          case '\"' => sb.append("\\\"")
          case c => sb.append(c)
        }
        sb.toString
      } + "\""
  }

  implicit val intShow = new Show[Int] {
    def show(i: Int) = i.toString
  }

  implicit val floatShow = new Show[Float] {
    def show(f: Float) = f + "f"
  }

  implicit val doubleShow = new Show[Double] {
    def show(d: Double) = d.toString
  }

  implicit val charShow = new Show[Char] {
    def show(c: Char) = "'" + (c match {
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '\'' => "\\'"
      case '\"' => "\\\""
      case c    => c
    }) + "'"
  }

  implicit def showList[T](implicit st: Show[T]) = new Show[List[T]] {
    def show(xs: List[T]) =
      if (xs.isEmpty) "Nil"
      else "List(" + xs.map(_.show).mkString(", ") + ")"
  }

  implicit val showNil = new Show[List[Nothing]] {
    def show(xs: List[Nothing]) = "Nil"
  }

  implicit def showOption[T](implicit st: Show[T]) = new Show[Option[T]] {
    def show(ot: Option[T]): String = ot match {
      case Some(t) => "Some("+ st.show(t) + ")"
      case none => "None"
    }
  }

  implicit val showNone = new Show[Option[Nothing]] {
    def show(n: Option[Nothing]) = "None"
  }

  implicit def showMap[K, V](implicit sk: Show[K], sv: Show[V]) = new Show[Map[K, V]] {
    def show(m: Map[K, V]) =
      "Map(" + m.map { case (k, v) => sk.show(k) + " -> " + sv.show(v) } .mkString (", ") + ")"
  }

  implicit def showMapOfNothing = new Show[Map[Nothing, Nothing]] {
    def show(m: Map[Nothing, Nothing]) = m.toString
  }
}
