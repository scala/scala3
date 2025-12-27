package scala.quoted.compiletime

/** Type which decides on the kind of parameter list represented by `MethodType`. */
sealed trait MethodTypeKind
object MethodTypeKind {

  /** Represents a parameter list without any implicitness of parameters, like (x1: X1, x2: X2, ...). */
  case object Plain extends MethodTypeKind

  /** Represents a parameter list with implicit parameters, like `(implicit X1, ..., Xn)`, `(using X1, ..., Xn)`, `(using x1: X1, ..., xn: Xn)`. */
  case object Implicit extends MethodTypeKind

  /** Represents a parameter list of a contextual method, like `(using X1, ..., Xn)` or `(using x1: X1, ..., xn: Xn)`. */
  case object Contextual extends MethodTypeKind

}
