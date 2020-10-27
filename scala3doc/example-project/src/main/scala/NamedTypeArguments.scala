package example

import experimental.namedTypeArguments

/**
  * Named Type Arguments: https://dotty.epfl.ch/docs/reference/other-new-features/named-typeargs.html
  */
object NamedTypeArguments {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit object listFunctor extends Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

  def test: Unit = {

    def fmap[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

    val result: List[Int] = fmap[F = List, A = Int, B = Int](List(1,2,3))(i => i + 1)

    println(result)

    // val notCompile = fmap[F = List, B = String](List(1,2,3))(i => i + 1)

    val compile: List[String] = fmap[F = List, B = String](List(1,2,3))(i => (i + 1).toString)

    println(compile)

  }

}
