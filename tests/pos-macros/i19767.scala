import scala.quoted.*

class ICons[K <: Singleton](val key: K)

def filterX(using Quotes): Unit =
  (??? : Expr[Any]) match
    case '{ $y : ICons[k1] } => '{ ICons($y.key) }