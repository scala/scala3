
### Subtyping Rules

An intesection type `A & B` is a subtype of its constituents `A`, and `B`

    A & B  <:  A
    A & B  <:  B

    T  <:  A & B    provided T <: A and T <: B

If `C` is a covariant type constructor,

    C[A & B]  =:=  C[A] & C[B]
    C[A & B]  <:  C[B]

