class Ctx:
  type T
  def get(): T = ???

trait Ops:
  private val ctx: Ctx = new Ctx
  private inline def get: ctx.T = ctx.get()
  inline def run: Any = get

object Impl extends Ops

class User:
  def use: Any = Impl.run
