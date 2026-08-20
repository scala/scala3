package pkg

trait Base:
  private[pkg] def foo: Base

trait Middle extends Base:
  override private[pkg] def foo: Middle = this
