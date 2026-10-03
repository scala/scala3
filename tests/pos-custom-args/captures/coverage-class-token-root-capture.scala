//> using options -Yexplicit-nulls

import language.experimental.captureChecking

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater

final class Box[T]
type AliasedBox = Box[Repro]
final class InvariantBox[T]
final class ContravariantBox[-T]
type RefinedBox = Box[Any] { type Element = Repro }
type CapturingObject = AnyRef^{caps.any}
type VariancePayload = (Repro, CapturingObject)

def factory[T](clazz: Class[T]): Box[T] = new Box[T]
def aliasedFactory(clazz: Class[Repro]): AliasedBox = new Box[Repro]
def invariantFactory[T](clazz: Class[T]): InvariantBox[T] = new InvariantBox[T]
def contravariantFactory[T](clazz: Class[T]): ContravariantBox[T] = new ContravariantBox[T]
def contravariantCaptureFactory(clazz: Class[?]): ContravariantBox[VariancePayload] =
  ???
def consumeContravariant(value: ContravariantBox[VariancePayload]): Unit = ()
def unionFactory(clazz: Class[Repro]): Repro | String = ???
def refinedFactory(clazz: Class[Repro]): RefinedBox = ???

class Repro(initialTail: AnyRef^)

def triggerInline(): Unit = identity[Box[Repro]](factory(classOf[Repro]))

def triggerPrebound(): Unit =
  val clazz: Class[Repro] = classOf[Repro]
  identity[Box[Repro]](factory(clazz))

def triggerAliased(): Unit =
  identity[AliasedBox](aliasedFactory(classOf[Repro]))

def triggerNestedVariance(): Unit =
  identity[InvariantBox[Repro]](invariantFactory(classOf[Repro]))
  identity[ContravariantBox[Repro]](contravariantFactory(classOf[Repro]))
  consumeContravariant(contravariantCaptureFactory(classOf[Repro]))

def triggerNestedStructuralTypes(): Unit =
  identity[Repro | String](unionFactory(classOf[Repro]))
  identity[RefinedBox](refinedFactory(classOf[Repro]))

trait StressSeq[+A]:
  def zipAll[A1 >: A, B](that: StressSeq[B]^, thisElem: A1, thatElem: B): StressSeq[(A1, B)]^{this, that}
  def map[B](f: A => B): StressSeq[B]^{this, f}

def triggerDeclarabilitySiblings[X](left: StressSeq[X]^, right: StressSeq[X]^): Unit =
  identity[(StressSeq[(Option[X], Option[X])]^{left, right}, Box[Repro])]((
    left.map(Option(_)).zipAll(right.map(Option(_)), None, None),
    factory(classOf[Repro])
  ))

final class MixedFuture[T](x: () => T)
final class MixedCollector[T, c^](futures: Seq[MixedFuture[T]^{c}]):
  def add(future: MixedFuture[T]^{c}): Unit = ()

def triggerMixedRootAndNested(cap: AnyRef^): Unit =
  val futures: IndexedSeq[MixedFuture[Repro]^{cap}] =
    (1 to 2).map(_ => new MixedFuture[Repro](() => { cap; ??? }))
  val collector = new MixedCollector[Repro, {cap}](futures)
  collector.add(new MixedFuture[Repro](() => { cap; ??? }))

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
