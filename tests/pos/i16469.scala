class Context {
  def normalMethod(): String = "normal"
  inline def inlineMethod(): String = "inline"
}

class Script(ctx: Context) {
  export ctx.*
  normalMethod()
  inlineMethod()
}

class MyScript(context: Context) extends Script(context) {
  normalMethod()
  inlineMethod()
}
