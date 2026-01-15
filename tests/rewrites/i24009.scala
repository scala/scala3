//> using options -Wunused:imports -rewrite -Wconf:name=UnusedSymbol&origin=p.C:s

package p:
  class C

package q:
  import p.C // nowarn and no rewrite

  class D
