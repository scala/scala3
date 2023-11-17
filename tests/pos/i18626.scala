trait Random[F1[_]]:
  def element[T1](list: Seq[T1]): F1[T1] = ???

trait Monad[F2[_]]:
  def map[A1, B1](fa: F2[A1])(f: A1 => B1): F2[B1]

object Monad:
  extension [F3[_]: Monad, A3](fa: F3[A3])
    def map[B3](f: A3 => B3): F3[B3] = ???

sealed trait Animal
object Cat extends Animal
object Dog extends Animal

type Mammal = Cat.type | Dog.type
val mammals: List[Mammal] = ???

class Work[F4[_]](random: Random[F4])(using mf: Monad[F4]):
  def result1: F4[Mammal] =
    mf.map(fa = random.element(mammals))(a => a)

  def result2: F4[Mammal] = Monad.map(random.element(mammals))(a => a)

  import Monad.*

  def result3: F4[Mammal] = random
    .element(mammals)
    .map { a =>
       a // was: Type Mismatch Error:
         //      Found:    (a : Animal)
         //      Required: Cat.type | Dog.type
}
