import scala.quoted.*

trait A:
  type T
  val b: B

trait B:
  type T
  def f: Unit

trait C0:
  type U
  val d: D0
trait D0:
  type U
  def h: Unit
object Macro:
  inline def generateCode: Unit = ${ generateCodeExpr }

  def generateCodeExpr(using Quotes): Expr[Unit] =
    '{
      $testLocalPathsGlobalClasses
      $testLocalPathsLocalClasses
    }

  def testLocalPathsGlobalClasses(using Quotes): Expr[Unit] =
    '{
      type T
      val a: A = ???
      ${
        val expr = '{
          val t: T = ???
          val aT: a.T = ???
          val abT: a.b.T = ???
          val aRef: a.type = ???
          aRef.b
          aRef.b.f
          val abRef: a.b.type = ???
          abRef.f
          ()
        }
        expr
      }
    }

  def testLocalPathsLocalClasses(using Quotes): Expr[Unit] =
    '{
      type U
      trait C extends C0:
        type U
        val d: D
      trait D extends D0:
        type U
        def h: Unit
      val c: C = ???
      ${
        val expr = '{
          val u: U = ???
          val cU: c.U = ???
          val cdU: c.d.U = ???
          val cRef: c.type = ???
          cRef.d
          cRef.d.h
          val cdRef: c.d.type = ???
          cdRef.h
          ()
        }
        expr
      }
    }
