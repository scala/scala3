//> using options -Yno-deep-subtypes:false
// Minimisation of tests/run-macros/i17257
// to understand how changes to match type reduction
// impacted this use of Tuple.IsMappedBy.
//
// During match type reduction
// if we do NOT simplify the case lambda parameter instances
// then this large tuple make TypeComparer breach LogPendingSubTypesThreshold
// which, under -Yno-deep-subtypes, crashes the compilation.
class C[+A]
def foo[T <: Tuple : Tuple.IsMappedBy[C]] = ???
def bar[X] = foo[(
  C[X], C[X], C[X], C[X], C[X], C[X], C[X], C[X], C[X], C[X],
  C[X], C[X], C[X], C[X], C[X], C[X], C[X], C[X], C[X], C[X],
  C[X], C[X], C[X],
)]
