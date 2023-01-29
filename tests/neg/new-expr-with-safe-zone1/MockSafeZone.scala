package scala.scalanative.safe

import language.experimental.captureChecking

class SafeZone {}

object SafeZone {
  final def apply[T](f: ({*} SafeZone) => T): T = f(open())

  final def open(): {*} SafeZone = new SafeZone
}

object SafeZoneCompat {
  def withSafeZone[T](sz: {*} SafeZone, obj: T): {sz} T = obj
}