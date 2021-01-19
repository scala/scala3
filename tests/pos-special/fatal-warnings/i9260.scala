package asts

enum Ast[-T >: Null] with
  case DefDef()

trait AstImpl[T >: Null] with
  type Ast = asts.Ast[T]
  type DefDef = Ast.DefDef[T]
end AstImpl

object untpd extends AstImpl[Null] with

  def DefDef(ast: Ast): DefDef = ast match
    case ast: DefDef => ast

end untpd
