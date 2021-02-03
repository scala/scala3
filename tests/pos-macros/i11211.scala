import scala.quoted._
def takeOptionImpl1[T](using Quotes, Type[T]): Unit = '{
  (??? : Option[T]) match
    case Some(t) => ???
}

def takeOptionImpl2[T](using Quotes, Type[T]): Unit = '{
  (??? : Option[T]) match
    case Some(_) | None => ???
}

def takeOptionImpl[T](o: Expr[Option[T]], default: Expr[T])(using Quotes, Type[T]): Expr[T] = '{
 $o match {
   case Some(t1) => t1
   case None: Option[T] => $default
 }
}

inline def takeOption[T](inline o: Option[T], inline default: T) = ${takeOptionImpl('o, 'default)}
