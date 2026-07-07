package zio

import zio.internal.stacktracer.Tracer

type Trace = Tracer.instance.Type & Tracer.Traced

final class ZLayer[-R, +E, +A]
object ZLayer:
  def environment[A](using Trace): ZLayer[A, Nothing, A] =
    new ZLayer[A, Nothing, A]

inline def broken: ZLayer[Any, Nothing, Any] =
  ${ zio.internal.macros.LayerMacros.brokenImpl }

package internal.stacktracer:
  import scala.quoted.*

  object Tracer:
    type Traced = Any
    inline given autoTrace: Tracer.instance.Type =
      ${ Macros.autoTraceImpl }
    val instance: Tracer = new Tracer:
      type Type = String

  sealed trait Tracer:
    type Type <: AnyRef

  object Macros:
    def autoTraceImpl(using Quotes): Expr[Tracer.instance.Type] =
      Expr("trace").asInstanceOf[Expr[Tracer.instance.Type]]

package internal.macros:
  import zio.*
  import scala.compiletime.*
  import scala.quoted.*

  object LayerMacros:
    def brokenImpl(using Quotes): Expr[ZLayer[Any, Nothing, Any]] =
      '{
        val trace = summonInline[Trace]
        ZLayer.environment[Any](using trace)
      }
