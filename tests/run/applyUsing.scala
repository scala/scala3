// `applyUsing` for arities 0-22 is found in the companion objects of `FunctionN`, so no import is needed.

class Config(val name: String)
class Logger(val prefix: String)

@main def Test(): Unit =
  given Config = Config("prod")
  given Logger = Logger("[app]")

  assert((() => "zero").applyUsing == "zero")
  assert(((c: Config) => c.name).applyUsing == "prod")
  assert(((c: Config, l: Logger) => l.prefix + c.name).applyUsing == "[app]prod")
