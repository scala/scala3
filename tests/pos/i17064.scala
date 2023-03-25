class HiddenInner[+O<:Outer](val outer:O){
}

class Outer{
  type Inner = HiddenInner[this.type]
}

val o : Outer       = new Outer
def a : o.Inner     = new o.Inner(o)
val b : Outer#Inner = a // DOES NOT COMPILE