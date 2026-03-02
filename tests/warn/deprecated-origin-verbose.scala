//> using options -deprecation -Wconf:any:verbose

package p:
  @deprecated("Old style", since="1.0")
  class C
  @deprecated("Bad style", since="1.0")
  class Crude

package q:
  import annotation.*
  import p.*
  class D extends C // warn
  class Oil extends Crude // warn
  @nowarn("""origin=p\.Crude""")
  class Language extends Crude // nowarn obvs
