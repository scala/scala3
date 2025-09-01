case class Container[A](val value: A) {
  def map[B](f: A => B): Container[B] = Container(f(value))
}

sealed trait Animal
case class Dog() extends Animal

def opOnDog(dog: Container[Dog]): Container[Animal] =
  for
    v <- dog
  yield v
