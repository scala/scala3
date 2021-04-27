import scala.compiletime.testing._

transparent inline def compileError(inline expr: String): Unit =
  println(typeCheckErrors(expr))

@main def Test = compileError(
  """compileError("1" * 2).check("")"""
)
