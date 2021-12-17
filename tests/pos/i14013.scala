import LightTypeTagInheritance._

trait LightTypeTagRef

object LightTypeTagInheritance {
  private final case class Ctx(self: LightTypeTagInheritance) {
    def next(): Ctx = Ctx(self)
  }
  private implicit final class CtxExt(private val ctx: Ctx) extends AnyVal {
    def isChild(selfT0: LightTypeTagRef, thatT0: LightTypeTagRef): Boolean = ctx.self.isChild(ctx.next())(selfT0, thatT0)
  }
}

class LightTypeTagInheritance {

  def isChild(s: LightTypeTagRef, t: LightTypeTagRef): Boolean = {
    isChild(new Ctx(this))(s, t)
  }

  private def isChild(ctx: Ctx)(s: LightTypeTagRef, t: LightTypeTagRef): Boolean = {
    ctx.isChild(s, t)
  }

}

object App extends App {
  println(LightTypeTagInheritance)
}


object Foo {
  case class Bar(i: Int)

  private implicit class BarOps(bar: Bar) {
    def twice = Bar(bar.i * 2)
  }
}

class Foo {
  def bar = Foo.Bar(1).twice
}

object App2 extends App {
  println((new Foo).bar)
}

object Foo2 {
  case class Bar(i: Int)

  private given BarOps: AnyRef with {
    extension (bar: Bar)
      def twice: Bar = Bar(bar.i * 2)
  }
}

class Foo2 {
  def bar = Foo2.Bar(1).twice
}

object App3 extends App {
  println((new Foo2).bar)
}

