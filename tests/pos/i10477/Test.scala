package gopher

import scala.util.Try

trait OChannel[F[_]:CpsAsyncMonad, A]:

   def awrite(a:A):F[Unit] =
     summon[CpsAsyncMonad[F]].adoptCallbackStyle(f =>
         addWriter(a, f)
     )

   inline def write(a:A): Unit = await(awrite(a))

   def addWriter(a:A, f: Try[Unit]=>Unit): Unit

