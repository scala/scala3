final class InlinedTypeApplications(val self: Any) extends AnyVal:
  def typeParams(using ctx: InlinedContext): Any = ctx.handle(self)
