import language.experimental.erasedDefinitions

class C(x: Int):
  erased val e = x

class D:
  val c = C(22)
  export c.*
  erased val x = e
