import language.experimental.captureChecking

def id[T](x: T): T = x

trait A:
  def t(): this.type = id(this)