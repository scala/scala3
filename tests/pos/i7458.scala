type Tr[+V1, +O1 <: V1]
extension [V2, O2 <: V2](tr: Tr[V2, O2]) def sl: Tr[V2, O2] = ???
def as[V3, O3 <: V3](tr: Tr[V3, O3]) : Tr[V3, O3] = tr.sl
