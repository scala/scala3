import scala.compiletime._
import scala.deriving._
import scala.quoted._
import scala.annotation.meta.field

object Macros{
    private def bindingFunctionBody[A: quoted.Type, B: quoted.Type](f: quoted.Expr[A => B])(using Quotes) =
      import quoted.quotes.reflect.*
      f.asTerm match
        case inlined @ Inlined(
              call,
              bindings,
              block @ Block(
                List(
                  defDef @ DefDef(
                    name,
                    paramss @ List(TermParamClause(List(param @ ValDef(paramName, paramTpt, _)))),
                    tpt,
                    Some(rhs)
                  )
                ),
                closureIdent
              )
            ) =>
          Inlined
            .copy(inlined)(
              call,
              bindings,
              '{ (a: A) =>
                ${
                  Block(
                    List(
                      ValDef
                        .copy(param)(paramName, paramTpt, Some('a.asTerm))
                        .changeOwner(Symbol.spliceOwner)
                    ),
                    '{
                      Binding(${ rhs.changeOwner(Symbol.spliceOwner).asExprOf[B] })
                    }.asTerm.changeOwner(Symbol.spliceOwner)
                  )
                    .asExprOf[Binding[B]]
                }: Binding[B]
              }.asTerm
            )
            .asExprOf[A => Binding[B]]
        case _ =>
          '{ (a: A) => Binding.Constant($f(a)): Binding[B] }
      end match
    end bindingFunctionBody

    def foreach[A: quoted.Type, U: quoted.Type](self: quoted.Expr[BindingSeq[A]], f: quoted.Expr[A => U])(using
        qctx: Quotes
    ): quoted.Expr[Unit] = '{ $self.foreachBinding(${ bindingFunctionBody(f) }).bind }
}
