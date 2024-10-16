import scala.annotation.experimental
import scala.quoted.*
import scala.annotation.tailrec

object FlatMap {
  @experimental inline def derived[F[_]]: FlatMap[F] = MacroFlatMap.derive
}
trait FlatMap[F[_]]{
  def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B]
}

@experimental
object MacroFlatMap:

  inline def derive[F[_]]: FlatMap[F] = ${ flatMap }

  def flatMap[F[_]: Type](using Quotes): Expr[FlatMap[F]] = '{
    new FlatMap[F]:
      def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] =
        ${ deriveTailRecM('{ a }, '{ f }) }
  }

  def deriveTailRecM[F[_]: Type, A: Type, B: Type](
      a: Expr[A],
      f: Expr[A => F[Either[A, B]]]
  )(using q: Quotes): Expr[F[B]] =
    import quotes.reflect.*

    val body: PartialFunction[(Symbol, TypeRepr), Term] = {
        case (method, tpe) => {
          given q2: Quotes = method.asQuotes
          '{
            def step(x: A): B = ???
            ???
          }.asTerm
        }
      }

    val term = '{ $f($a) }.asTerm
    val name = Symbol.freshName("$anon")
    val parents = List(TypeTree.of[Object], TypeTree.of[F[B]])
  
    extension (sym: Symbol) def overridableMembers: List[Symbol] = 
      val member1 = sym.methodMember("abstractEffect")(0)
      val member2 = sym.methodMember("concreteEffect")(0)
      def meth(member: Symbol) = Symbol.newMethod(sym, member.name, This(sym).tpe.memberType(member), Flags.Override, Symbol.noSymbol)
      List(meth(member1), meth(member2))
    
    val cls = Symbol.newClass(Symbol.spliceOwner, name, parents.map(_.tpe), _.overridableMembers, None)

    def transformDef(method: DefDef)(argss: List[List[Tree]]): Option[Term] =
      val sym = method.symbol
      Some(body.apply((sym, method.returnTpt.tpe)))

    val members = cls.declarations
      .filterNot(_.isClassConstructor)
      .map: sym =>
        sym.tree match
          case method: DefDef => DefDef(sym, transformDef(method))
          case _ => report.errorAndAbort(s"Not supported: $sym in ${sym.owner}")

    val newCls = New(TypeIdent(cls)).select(cls.primaryConstructor).appliedToNone
    Block(ClassDef(cls, parents, members) :: Nil, newCls).asExprOf[F[B]]
