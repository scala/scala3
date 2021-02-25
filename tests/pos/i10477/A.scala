package gopher

import scala.util.Try

def await[F[_],T](f:F[T])(using am:CpsAsyncMonad[F]):T = ???

trait CpsAsyncMonad[F[_]]:

   def adoptCallbackStyle[A](source: (Try[A]=>Unit) => Unit): F[A]

trait IChannel[F[_]:CpsAsyncMonad, A]:

   def aread:F[A] =
      summon[CpsAsyncMonad[F]].adoptCallbackStyle(f => addReader(f))

   inline def read: A = await(aread)

   def addReader(f: Try[A]=>Unit): Unit


trait IOChannel[F[_]:CpsAsyncMonad,I,O] extends IChannel[F,I] with OChannel[F,O]

