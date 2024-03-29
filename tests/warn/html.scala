

object HTML:
  type AttrArg = AppliedAttr | Seq[AppliedAttr]
  opaque type AppliedAttr = String
  opaque type AppliedTag = StringBuilder

  case class Tag(name: String):
    def apply(attrs: AttrArg*): AppliedTag = {
      val sb = StringBuilder()
      sb.append(s"<$name")
      attrs.filter(_ != Nil).foreach{
        case s: Seq[AppliedAttr] =>
          s.foreach(sb.append(" ").append)
        case s: Seq[Int] => // warn
        case e: AppliedAttr =>
          sb.append(" ").append(e)
      }
      sb
    }
