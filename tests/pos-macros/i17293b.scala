import scala.quoted.*

trait OuterTrait { self =>
  trait X

  def exampleMacro[T <: self.type: Type](expr: Expr[T])(using Quotes): Expr[self.X] = {
    '{
      val prefix: T = ${ expr }
      new prefix.X {}
    }
  }
}