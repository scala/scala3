
transparent inline def inScope[T](inline expr: Context ?=> T): T =
  val fn = Macros.transformContextLambda[T](expr)
  fn(new Context {})

@main def Test = {
  inScope {
    Scope.spawn[Unit] { () }
  }
}
