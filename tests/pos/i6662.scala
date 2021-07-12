object opt:
    opaque type Opt[A >: Null] = A
    object Opt:
        inline def unOpt[A >: Null](x: Opt[A]): A = x
        inline def apply[A >: Null](x: A): Opt[A] = x
        inline def some[A >: Null](x: A): Opt[A] = x
        inline def none[A >: Null]: Opt[A] = null
        inline def fromOption[A >: Null](x: Option[A]) = x.orNull

import opt.Opt
extension [A >: Null](x: Opt[A])
    inline def nonEmpty : Boolean = x.get != null
    inline def isEmpty  : Boolean = x.get == null
    inline def isDefined: Boolean = x.nonEmpty
    inline def get      : A       = Opt.unOpt(x)

@main def Test =
  val x: Opt[String] = Opt.some("abc")
  assert(x.nonEmpty)
  val y: String = Opt.unOpt(x)
