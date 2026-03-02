// https://github.com/scala/scala3/issues/12663

final class HookComponentBuilder[Ctx, CtxFn[_]] {
  def asd[A](f: Ctx => A): A = ???
  def asd[A](f: CtxFn[A]): A = ???
}

object HookCtx {
  case class P1[P, H1](props: P, hook1: H1)
}

object HookCtxFn {
  sealed trait P1[P, H1] { type Fn[A] = (P, H1) => A }
}

object Test {
  val b: HookComponentBuilder[
    HookCtx.P1[String, Int],
    HookCtxFn.P1[String, Int]#Fn
  ] = ???

  b.asd($ => $.props.length + $.hook1)
  b.asd((props, hook1) => props.length + hook1)
}

final class HookComponentBuilder2[Ctx, CtxFn[_]] {
  def asd[A](f: Ctx => A): A = ???
  def asd[A](f: CtxFn[A]): A = ???
}

object HookCtx2 {
  case class P1[P, H1](props: P, hook1: H1)
}

object HookCtxFn2 {
  type P1[P, H1] = [A] =>> (P, H1) => A
}

object Test2 {
  val b: HookComponentBuilder2[
    HookCtx2.P1[String, Int],
    HookCtxFn2.P1[String, Int]
  ] = ???

  b.asd($ => $.props.length + $.hook1)
  b.asd((props, hook1) => props.length + hook1)
}

final class Builder[CtxFn[_]] {
  def asd[A](f: Int => A): A = ???
  def asd[A](f: CtxFn[A]): A = ???
}

object Test3 {
  val b1: Builder[[Z] =>> (String, Int) => Z] = ???
  b1.asd(identity)
  b1.asd(_.length + _)

  sealed trait Scala2TL { type F[Z] = (String, Int) => Z }
  val b2: Builder[Scala2TL#F] = b1
  b2.asd(identity)
  b2.asd(_.length + _)

  type Scala3TL = [Z] =>> (String, Int) => Z
  val b3: Builder[Scala3TL] = b1
  b3.asd(identity)
  b3.asd(_.length + _)

  val b4: Builder[({ type F[Z] = (String, Int) => Z })#F] = b1
  b4.asd(identity)
  b4.asd(_.length + _)
}
