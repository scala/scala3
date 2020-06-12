// `case _ => expr` in a match expression should be equivalant to
// `case _: Any => expr`. Likewise, in a match type, `case _ => T`
// should be equivalant to `case Any => T`.

object Test0 {
  type M[X] =            X match { case String => Int   case Any => String }
  def m[X](x: X): M[X] = x match { case _: String => 1  case _: Any => "s" }
}

object Test1 {
  type M[X] =            X match { case String => Int   case Any => String }
  def m[X](x: X): M[X] = x match { case _: String => 1  case _ => "s"      }
}

object Test2 {
  type M[X] =            X match { case String => Int   case _ => String   }
  def m[X](x: X): M[X] = x match { case _: String => 1  case _: Any => "s" }
}

object Test3 {
  type M[X] =            X match { case String => Int   case _ => String   }
  def m[X](x: X): M[X] = x match { case _: String => 1  case _ => "s"      }
}
