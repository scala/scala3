//> using options -language:experimental.specializedTraits

@main def Test =
    assert(B.a.foo == "A$impl$scala$Int")
    assert(C.a.foo == "A$impl$scala$Int")
