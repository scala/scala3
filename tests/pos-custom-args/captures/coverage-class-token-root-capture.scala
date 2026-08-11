//> using options -Yexplicit-nulls

import language.experimental.captureChecking

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

final class Box[T]

def factory[T](clazz: Class[T]): Box[T] = new Box[T]

class Repro(initialTail: AnyRef^)

def triggerInline(): Unit = identity[Box[Repro]](factory(classOf[Repro]))

def triggerPrebound(): Unit =
  val clazz: Class[Repro] = classOf[Repro]
  identity[Box[Repro]](factory(clazz))

abstract class WildcardRepro[+A](initialTail: (AnyRef | Null)^):
  def makeTailUpdater: TailUpdater =
    new TailUpdater(
      AtomicReferenceFieldUpdater.newUpdater(
        classOf[WildcardRepro[?]],
        classOf[AnyRef],
        "_tail"
      )
    )

final class TailUpdater(u: AtomicReferenceFieldUpdater[WildcardRepro[?], AnyRef])
