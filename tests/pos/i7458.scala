type Tr[+V1, +O1 <: V1]
def [V2, O2 <: V2](tr: Tr[V2, O2]) sl: Tr[V2, O2] = ???
def as[V3, O3 <: V3](tr: Tr[V3, O3]) : Tr[V3, O3] = tr.sl
