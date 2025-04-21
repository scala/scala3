//> using options -Xlint:type-parameter-shadow -Wunused:all

class F[X, M[N[X]]]: // warn
  private def x[X] = toString // warn // warn
