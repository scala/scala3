class CC

def test(cap1: CC^) =

  class Ref[T](init: T):
    private var v: T = init
    def setV(x: T): Unit = v = x
    def getV: T = v

  val r = Ref((x: String) => x)

  def scope(cap3: CC^) =
    def g(x: String): String = if cap3 == cap3 then "" else "a"
    r.setV(g) // error
  ()

/*
  Explicit:
    cap is local root of enclosing method or class, can be overridden by qualifying it.
    i.e. cap[name]

  On method  instantiation: All uses of cap --> cap of caller
  On class instantiation: All uses of cap, or local cap of clsss --> cap of caller

  Alternative solution: root variables
    - track minimal & maximal level
    - updated via subsumption tests, root added handler for prefix/member

  roots: Implicitly: outer <: inner

  def withFile[T]((local: Root) ?=> op: File^{local}) => T]): T
*/
