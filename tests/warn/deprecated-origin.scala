//> using options -deprecation -Wconf:origin=p\.C$:s

package p:
  @deprecated("Old style", since="1.0")
  class C
  @deprecated("Bad style", since="1.0")
  class Crude

package q:
  import annotation.*
  import p.*
  class D extends C // nowarn - C$ pattern avoids matching Crude
  class Oil extends Crude // warn
  @nowarn("""origin=p\.Crude""")
  class Language extends Crude // nowarn obvs
