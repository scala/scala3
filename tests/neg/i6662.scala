opaque type Opt[A >: Null] = A

inline def [A >: Null](x: Opt[A]) nonEmpty: Boolean = x.get != null // error: Implementation restriction
inline def [A >: Null](x: Opt[A]) isEmpty: Boolean = x.get == null // error: Implementation restriction
inline def [A >: Null](x: Opt[A]) isDefined: Boolean = x.nonEmpty // error: Implementation restriction
inline def [A >: Null](x: Opt[A]) get: A = Opt.unOpt(x) // error: Implementation restriction

object Opt
{
    inline def unOpt[A >: Null](x: Opt[A]): A = x // error: Implementation restriction
    inline def apply[A >: Null](x: A): Opt[A] = x // error: Implementation restriction
    inline def some[A >: Null](x: A): Opt[A] = x // error: Implementation restriction
    inline def none[A >: Null]: Opt[A] = null // error: Implementation restriction
    inline def fromOption[A >: Null](x: Option[A]) = x.orNull // error: Implementation restriction
}
