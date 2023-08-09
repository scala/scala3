import scala.quoted.*

trait OuterTrait {
  trait X
}

def exampleMacro[T <: OuterTrait: Type](expr: Expr[T])(using Quotes): Expr[OuterTrait#X] = {
  '{
    val prefix: T = ${ expr }
    new prefix.X {}
  }
}
