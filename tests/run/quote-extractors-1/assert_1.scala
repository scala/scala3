import scala.quoted._
import scala.tasty._


object Macro {

  inline def transformAndPrint(code: => Any): Unit = ~transformImpl('(code))

  private def transformImpl(code: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._

    val transformer = new Transformer {
      override def postTransform[T](c: Expr[T])(implicit t: quoted.Type[T]): Expr[T] = c match {
        // case app0 @@ '{ (~prefix).apply() }
        case Inspect(app0: SealedApplication0[T]) =>
         '{
            val pre: ~app0.prefix.tpe = ~app0.prefix.expr
            ~app0('(pre))
          }
        // case app1 @@ '{ (~prefix).apply(~arg) }
        case Inspect(app1: SealedApplication1[T]) =>
         '{
            val pre: ~app1.prefix.tpe = ~app1.prefix.expr
            val arg: ~app1.arg.tpe = ~app1.arg.expr
            ~app1('(pre))('(arg))
          }
        // case whileDo @@ '{ while (~cond) ~body }
        case Inspect(whileDo @ SealedWhile(cond, body)) =>
          whileDo (cond.expr) {
            '(???)
          }
        case _ =>
          c
      }
    }

    val code2: Expr[Any] = transformer.transform(code.unseal.underlyingArgument.seal.asExprOf[Any])

    implicit val toolbox: Toolbox = Toolbox.make
   '{
      println(~code.show.toExpr)
      println(~code2.show.toExpr)
      println()
    }
  }

  /// LIBRARY CODE

  class Transformer(implicit refl: Reflection) {

    def postTransform[T](c: Expr[T])(implicit t: quoted.Type[T]): Expr[T] = c

    def transform[T](c: Expr[T])(implicit t: quoted.Type[T]): Expr[T] = {
      import refl._
      c match {
        // case '{ if (~cond) ~thenp else ~elsep }
        case Inspect(iF @ SealedIf(cond, thenp, elsep)) =>
          postTransform(iF(transformSealed(cond).expr)(transformSealed(thenp).expr, transformSealed(elsep).expr))
        // case '{ (~prefix).apply() }
        case Inspect(app0: SealedApplication0[T]) =>
          postTransform(app0(transformSealed(app0.prefix).expr))
        // case '{ (~prefix).apply(~arg) }
        case Inspect(app1: SealedApplication1[T]) =>
          postTransform(app1(transformSealed(app1.prefix).expr)(transformSealed(app1.arg).expr))
        // case '{ while (~cond) ~body }
        case Inspect(whileDo @ SealedWhile(cond, body)) =>
          postTransform(whileDo(transformSealed(cond).expr)(transformSealed(body).expr))
        // case '{ ~(stats: _*); ~expr }
        case Inspect(block: SealedBlock[T]) =>
          postTransform(block(block.stats.map(x => transformSealed(x).expr), transformSealed(block.last).expr))
        case _ =>
          c
      }
    }

    def transformSealed[T](c: Sealed): Sealed { type Tpe = c.Tpe } = Sealed(transform(c.expr)(c.tpe))(c.tpe)

  }


  object Inspect {
    def unapply[T: Type](arg: Expr[T])(implicit refl: Reflection): Option[SplittedExpr[T]] = {
      import refl._
      arg.unseal match {
        case Term.IsIf(ifExpr) =>
          Some(new SealedIf {
            val _1: SealedBy[Boolean] = ifExpr.cond.seal.asInstanceOf[SealedBy[Boolean]]
            val _2: SealedBy[T] = ifExpr.thenp.seal.asInstanceOf[SealedBy[T]]
            val _3: SealedBy[T] = ifExpr.elsep.seal.asInstanceOf[SealedBy[T]]
            def apply(cond: Expr[Boolean])(thenp: Expr[T], elsep: Expr[T]): Expr[T] =
              Term.If.copy(ifExpr)(cond.unseal, thenp.unseal, elsep.unseal).seal.asExprOf[T]
          })
        case Term.IsWhile(whileDo) =>
          Some(new SealedWhile {
            val _1: SealedBy[Boolean] = whileDo.cond.seal.asInstanceOf[SealedBy[Boolean]]
            val _2: Sealed = whileDo.body.seal
            def apply(cond: Expr[Boolean])(body: Expr[Any]): Expr[T & Unit] =
              Term.While.copy(whileDo)(cond.unseal, body.unseal).seal.asExprOf[T & Unit]
          })
        case Term.IsBlock(block) =>
          // TODO Use let expression instead?
          val statements = block.statements.collect { case IsTerm(x) => x.seal }
          if (statements.size != block.statements.size) {
            None
          } else {
            Some(new SealedBlock {
              val stats: List[Sealed] = statements
              val last: SealedBy[T] = block.expr.seal.asInstanceOf[SealedBy[T]]
              def apply(stats: List[Expr[_]], last: Expr[T]): Expr[T] =
                Term.Block.copy(block)(stats.map(_.unseal), last.unseal).seal.asExprOf[T]
            })
          }
        case Term.IsSelect(sel @ Term.Select(pre, op)) =>
          Some(new SealedApplication0 {
            val prefix: Sealed = pre.seal
            def apply(prefix: Expr[this.prefix.Tpe]): Expr[T] =
              Term.Select.copy(sel)(prefix.unseal, op).seal.asExprOf[T]
          })
        case Term.Apply(Term.IsSelect(sel @ Term.Select(pre, op)), args) =>
          args match {
            case a1 :: Nil =>
              Some(new SealedApplication1 {
                val prefix: Sealed = pre.seal
                val arg: Sealed = a1.seal
                def apply(prefix: Expr[this.prefix.Tpe])(arg: Expr[this.arg.Tpe]): Expr[T] =
                  Term.Apply(Term.Select.copy(sel)(prefix.unseal, op), arg.unseal :: Nil).seal.asExprOf[T]
              })
            case a1 :: a2 :: Nil =>
              Some(new SealedApplication2 {
                val prefix: Sealed = pre.seal
                val arg1: Sealed = a1.seal
                val arg2: Sealed = a2.seal
                def apply(prefix: Expr[this.prefix.Tpe])(arg1: Expr[this.arg1.Tpe], arg2: Expr[this.arg2.Tpe]): Expr[T] =
                  Term.Apply(Term.Select.copy(sel)(prefix.unseal, op), arg1.unseal :: arg2.unseal :: Nil).seal.asExprOf[T]
              })
          }
        case tree =>
          None
      }
    }
  }

  type SealedBy[X] = Sealed { type Tpe <: X }

  sealed trait SplittedExpr[T] { self =>
    def get: self.type = self
    def isEmpty: Boolean = false
  }

  trait SealedModule[U[T] <: SplittedExpr[T]] {
    def unapply[T](arg: U[T]): arg.type = arg
  }

  object SealedIf extends SealedModule[SealedIf]
  sealed trait SealedIf[T] extends SplittedExpr[T] {
    /** Condition */
    def _1: SealedBy[Boolean]
    /** `then` part */
    def _2: SealedBy[T]
    /** `else` part */
    def _3: SealedBy[T]
    /** `if` reconstructor */
    def apply(cond: Expr[Boolean])(thenp: Expr[T], elsep: Expr[T]): Expr[T]
  }

  object SealedWhile extends SealedModule[SealedWhile]
  trait SealedWhile[T] extends SplittedExpr[T] {
    /** Condition of the loop */
    def _1: SealedBy[Boolean]
    /** Body of the loop */
    def _2: Sealed
    /** Loop reconstructor */
    def apply(cond: Expr[Boolean])(body: Expr[Any]): Expr[T & Unit]
  }

  // TODO use let expression instead?
  trait SealedBlock[T] extends SplittedExpr[T] {
    val stats: List[Sealed]
    val last: SealedBy[T]
    def apply(stats: List[Expr[_]], last: Expr[T]): Expr[T]
  }

  trait SealedApplication0[T] extends SplittedExpr[T] {
    val prefix: Sealed
    def apply(prefix: Expr[this.prefix.Tpe]): Expr[T]
  }

  trait SealedApplication1[T] extends SplittedExpr[T] {
    val prefix: Sealed
    val arg: Sealed
    def apply(prefix: Expr[this.prefix.Tpe])(arg: Expr[this.arg.Tpe]): Expr[T]
  }

  trait SealedApplication2[T] extends SplittedExpr[T] {
    val prefix: Sealed
    val arg1: Sealed
    val arg2: Sealed
    def apply(prefix: Expr[this.prefix.Tpe])(arg1: Expr[this.arg1.Tpe], arg2: Expr[this.arg2.Tpe]): Expr[T]
  }
}