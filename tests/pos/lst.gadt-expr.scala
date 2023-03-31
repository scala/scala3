class Lst[+T](val elems: Any) extends AnyVal:
  override def equals(that: Any) = that match
    case that: Lst[t] => eqLst(that)
    case _ => false

  def eqLst[U](that: Lst[U]) = elems.asInstanceOf[AnyRef] eq that.elems.asInstanceOf[AnyRef]
