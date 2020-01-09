opaque type Opt[A >: Null] = A

inline def (x: Opt[A]) nonEmpty[A >: Null]: Boolean = x.get != null // error: Implementation restriction
inline def (x: Opt[A]) isEmpty[A >: Null]: Boolean = x.get == null // error: Implementation restriction
inline def (x: Opt[A]) isDefined[A >: Null]: Boolean = x.nonEmpty // error: Implementation restriction
inline def (x: Opt[A]) get[A >: Null]: A = Opt.unOpt(x) // error: Implementation restriction

object Opt
{
    inline def unOpt[A >: Null](x: Opt[A]): A = x // error: Implementation restriction
    inline def apply[A >: Null](x: A): Opt[A] = x // error: Implementation restriction
    inline def some[A >: Null](x: A): Opt[A] = x // error: Implementation restriction
    inline def none[A >: Null]: Opt[A] = null // error: Implementation restriction
    inline def fromOption[A >: Null](x: Option[A]) = x.orNull // error: Implementation restriction
}
