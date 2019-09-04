package scala.quoted

/** Excetion thrown when an Expr or Type is used ouside of the scope where it is valid */
class ScopeException(msg: String) extends Exception(msg)
