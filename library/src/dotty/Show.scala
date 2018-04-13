package dotty

trait Show[T] {
  def show(t: T): String
}

trait LowPrioShow {
  implicit def defaultShow[T]: Show[T] = new Show[T] {
    def show(x: T) = x.toString
  }
}

object Show extends LowPrioShow {
  /** This class implements pimping of all types to provide a show method.
    * Currently it is quite permissive, if there's no instance of `Show[T]` for
    * any `T`, we default to `T#toString`.
    */
  implicit class ShowValue[V](val v: V) extends AnyVal {
    def show(implicit ev: Show[V]): String = if (v == null) "null" else ev.show(v)
  }

  /** Adds escaping backslashes in a string so that it could be put in source code.
    * For example, a newline character is shown as '\n' (without the quotes).
    * Chars that don't have to be escaped are simply converted to a string.
    * @param c the char to be escaped
    * @return a string describing how to escape the give char in source code.
    */
  private def escapeChar(c: Char): String = c match {
    // From 2.12 spec, `charEscapeSeq`:
    // ‘\‘ (‘b‘ | ‘t‘ | ‘n‘ | ‘f‘ | ‘r‘ | ‘"‘ | ‘'‘ | ‘\‘)
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '\'' => "\\'"
    case '\"' => "\\\""
    case '\\' => "\\\\"
    case c    => c.toString
  }

  implicit val stringShow: Show[String] = new Show[String] {
    def show(str: String) = str.flatMap(escapeChar).mkString("\"", "", "\"")
  }

  implicit val floatShow: Show[Float] = new Show[Float] {
    def show(f: Float) = f + "f"
  }

  implicit val charShow: Show[Char] = new Show[Char] {
    def show(c: Char) = s"'${escapeChar(c)}'"
  }

  implicit def showList[T](implicit st: Show[T]): Show[List[T]] = new Show[List[T]] {
    def show(xs: List[T]) =
      if (xs.isEmpty) "List()"
      else "List(" + xs.map(_.show).mkString(", ") + ")"
  }

  implicit def arrayShow[T](implicit st: Show[T]): Show[Array[T]] = new Show[Array[T]] {
    def show(xs: Array[T]): String =
      if (xs.isEmpty) "Array()"
      else "Array(" + xs.map(_.show).mkString(", ") + ")"
  }

  implicit def showOption[T](implicit st: Show[T]): Show[Option[T]] = new Show[Option[T]] {
    def show(ot: Option[T]): String = ot match {
      case Some(t) => "Some("+ st.show(t) + ")"
      case none => "None"
    }
  }

  implicit def showSome[T](implicit st: Show[T]): Show[Some[T]] = new Show[Some[T]] {
    def show(ot: Some[T]): String = "Some("+ st.show(ot.get) + ")"
  }

  implicit def showMap[K,V](implicit sk: Show[K], sv: Show[V]): Show[Map[K,V]] = new Show[Map[K,V]] {
    def show(m: Map[K, V]) =
      "Map(" + m.map { case (k, v) => sk.show(k) + " -> " + sv.show(v) } .mkString (", ") + ")"
  }
}
