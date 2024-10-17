//> using options -Wshadow:type-parameter-shadow -Wunused:all

class F[X, M[N[X]]]:
  private def x[X] = toString // warn // warn

// the first is spurious
// at 3: Type parameter X for type M shadows the type defined by type X in class F
// at 4: Type parameter X for method x shadows the type defined by type X in class F
// at 4: unused private member
