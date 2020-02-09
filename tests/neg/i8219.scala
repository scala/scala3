object O{
  opaque type T[X <: AnyKind] = X
}
def m(x: O.T[AnyKind]) = x // error