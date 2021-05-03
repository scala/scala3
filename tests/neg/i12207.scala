package example

extension [T](t: T) inline def pi[P <: Tuple](using P): T = ???

inline def env[P <: Tuple, T](op: P ?=> T): P ?=> T = op

@main def Test =
  env { pi[String] } // error // error
