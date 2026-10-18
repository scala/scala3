type L[F[+_]] = [S, X] =>> S match
  case Unit => F[X]

final class Tree[G[_, +_]]

def bind[F[+_], G[+_]](p: Tree[L[F]]): Tree[L[[A] =>> F[A] | G[A]]] = ???

val x: Tree[L[[A] =>> Nothing]] = Tree()
val y: Tree[L[[A] =>> Nothing]] = bind(x)
