// https://github.com/scala/scala3/issues/25806
import scala.util.boundary
import scala.util.boundary.Label

type Direct[A] = Label[Either[String, Nothing]] ?=> A

object Mode {
  trait FailFast[F[+x]] {
    def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
    def pure[A](value: A): F[A]
  }
}

object Transformer {
  trait Fallible[F[+x], Source, Dest] {
    def transform(value: Source): F[Dest]
  }
}

object LabelMode extends Mode.FailFast[Direct] {

  override def flatMap[A, B](
      fa: Direct[A],
      f: A => Direct[B]
  ): Direct[B] = f(fa)

  override def pure[A](value: A): Direct[A] = value
}

final case class Nat private (value: Int)

object Nat {
  def make(value: Int)(using label: Label[Either[String, Nothing]]): Nat =
    if value < 0 then boundary.break(Left(s"Haha no - $value is not a proper Nat")) else Nat(value)

  given t: Transformer.Fallible[Direct, Int, Nat] with {
    override def transform(source: Int): Direct[Nat] = make(source)
  }
}

case class Person(age: Nat, somethingElse: Nat, moreStuff: String)
case class WirePerson(age: Int, somethingElse: Int, moreStuff: String)

inline def transformWirePerson[F[+x]](wire: WirePerson)(using F: Mode.FailFast[F], T: Transformer.Fallible[F, Int, Nat]): F[Person] = {
  F.flatMap(
    T.transform(wire.age),
    age =>
      F.flatMap(
        T.transform(wire.somethingElse),
        smthElse => F.pure(Person(age, smthElse, wire.moreStuff))
      )
  )
}

@main def main25806 = {
  given LabelMode.type = LabelMode
  val res =
    boundary {
      val d = transformWirePerson(WirePerson(1, 2, "asd"))
      d
    }

  println(res)
}
