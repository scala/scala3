package scala.quoted

class Type[T] extends Quoted {
  type unary_~ = T
}

