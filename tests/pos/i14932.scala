trait Core {
  class Base[T]()
}

class Module(val core: Core) {
  object Indirection {
    class Extension[T]() extends core.Base[T]()
  }
}