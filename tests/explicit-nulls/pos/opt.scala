
type Opt[+A] = A | Null
object Opt:

  def unapply[A](o: Opt[A]): Option[A] =
    if o != null then Some(o.asInstanceOf[o.type & A])
    else None

end Opt