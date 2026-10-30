class Ctx:
  type T
  def get(): T = ???

class Svc(ctx: Ctx):
  private inline def get: ctx.T = ctx.get()
  private def run = get
