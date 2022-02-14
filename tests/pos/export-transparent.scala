class Container(args: Any*):
  object Internal

object Test:
  transparent inline def myC(args: Any*): Container = Container(args)

  export myC(1).*
