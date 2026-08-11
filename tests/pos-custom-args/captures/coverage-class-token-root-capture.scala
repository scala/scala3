//> using options -Yexplicit-nulls

import language.experimental.captureChecking

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

final class Box[T]
type AliasedBox = Box[Repro]

def factory[T](clazz: Class[T]): Box[T] = new Box[T]

class Repro(initialTail: AnyRef^)

def triggerInline(): Unit = identity[Box[Repro]](factory(classOf[Repro]))

def triggerPrebound(): Unit =
  val clazz: Class[Repro] = classOf[Repro]
  identity[Box[Repro]](factory(clazz))

def triggerAliased(): Unit =
  identity[AliasedBox](factory(classOf[Repro]))

final class MixedFuture[T](x: () => T)
final class MixedCollector[T, c^](futures: Seq[MixedFuture[T]^{c}]):
  def add(future: MixedFuture[T]^{c}): Unit = ()

def triggerMixedRootAndNested(): Unit =
  val futures: IndexedSeq[MixedFuture[Repro]] =
    (1 to 2).map(_ => new MixedFuture[Repro](() => ???))
  val collector = new MixedCollector[Repro, {}](futures)
  collector.add(new MixedFuture[Repro](() => ???))

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
