package scala.quoted

class Type[T] extends Quoted {
  type unary_~ = T
}

/** Some basic type tags, currently incomplete */
object Type {
  implicit def IntTag: Type[Int] = new Type[Int]
  implicit def BooleanTag: Type[Boolean] = new Type[Boolean]
}
