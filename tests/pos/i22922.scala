abstract class Computation[+A, -U]
type !![+A, -U] = Computation[A, U]
type Const[C] = [_] =>> C

final class EffectImpl[Fx]:
  sealed trait ThisInterpreter extends Interpreter.Unsealed:
    final override type Elim = Fx
  abstract class Stateless[F[+_], G[+_], Fx]
      extends Interpreter.Stateless[F, G, Fx]
      with ThisInterpreter

trait Effect:
  val impl: EffectImpl[this.type] = ???

trait SourceEffect[O] extends Effect:
  abstract class Stateless[U] extends StatelessReturn[Unit, U]
  abstract class StatelessReturn[R, U] extends impl.Stateless[Const[Unit], Const[R], U]

sealed trait Handler[From[+_], To[+_], Elim, Intro]:
  final def handle[V] = new HandleSyntax[V]
  final class HandleSyntax[V]:
    def apply[A, W](comp: From[A] !! W)(using CanPartiallyHandle[V, W, Elim]): To[A] !! (V & Intro) = ???

sealed trait CanPartiallyHandle[U, V, W] // missing in StreamImpl.map
object CanPartiallyHandle:
  given [U, V, W](using (W & U) <:< V): CanPartiallyHandle[U, V, W] = ???

sealed trait Interpreter:
  type From[+A]
  type To[+A]
  type Elim
  type Intro

  final def toHandler: Handler[From, To, Elim, Intro] = ???
object Interpreter:
  trait Unsealed extends Interpreter
  abstract class Stateless[F[+_], G[+_], Fx] extends Interpreter:
    final override type From[+A] = F[A]
    final override type To[+A] = G[A]
    final override type Intro = Fx

object Syntax:
  extension [U](comp: Unit !! U)
    def asStream[A, V](fx: SourceEffect[A])(using (fx.type & V) =:= U): Stream[A, V] = ???

sealed abstract class Stream[+A, -U]:
  def map[B](f: A => B): Stream[B, U]

import Syntax.*
final case class StreamImpl[A, U](Fx: SourceEffect[A])(val compute: Unit !! (U & Fx.type))
    extends Stream[A, U]:
  type Fx = Fx.type
  override def map[B](f: A => B): Stream[B, U] =
    case object Fx2 extends SourceEffect[B]
    new Fx.Stateless[Fx2.type] {}.toHandler
      .handle(compute)
      .asStream(Fx2) // error
