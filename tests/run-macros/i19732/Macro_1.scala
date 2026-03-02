// package dummy

import scala.quoted.*

trait Defaults[A]:
  def defaults: Map[String, Any]

object Defaults:
  inline def derived[A <: Product]: Defaults[A] = ${ defaultsImpl[A] }

  def defaultsImpl[A <: Product: Type](using Quotes): Expr[Defaults[A]] =
    '{
      new Defaults[A]:
        def defaults: Map[String, Any] = ${ defaultParmasImpl[A] }
    }

def defaultParmasImpl[T](using quotes: Quotes, tpe: Type[T]): Expr[Map[String, Any]] =
  import quotes.reflect.*

  TypeRepr.of[T].classSymbol match
    case None => '{ Map.empty[String, Any] }
    case Some(sym) =>
      val comp = sym.companionClass
      val mod = Ref(sym.companionModule)
      val names =
        for p <- sym.caseFields if p.flags.is(Flags.HasDefault)
        yield p.name
      val namesExpr: Expr[List[String]] =
        Expr.ofList(names.map(Expr(_)))

      val body = comp.tree.asInstanceOf[ClassDef].body
      val idents: List[Ref] =
        for
          case deff @ DefDef(name, _, _, _) <- body
          if name.startsWith("$lessinit$greater$default")
        yield mod.select(deff.symbol)
      val typeArgs = TypeRepr.of[T].typeArgs
      val identsExpr: Expr[List[Any]] =
        if typeArgs.isEmpty then Expr.ofList(idents.map(_.asExpr))
        else Expr.ofList(idents.map(_.appliedToTypes(typeArgs).asExpr))

      '{ $namesExpr.zip($identsExpr).toMap }
