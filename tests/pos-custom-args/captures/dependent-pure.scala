import language.experimental.captureChecking
class ContextCls
type Context = ContextCls^

class Filtered(p: (c: Context) ?-> () ->{c} Boolean) extends caps.Pure
