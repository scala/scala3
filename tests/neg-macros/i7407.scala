def qc(using s: quoted.Scope) = println(s)
inline def g = qc  // error: no implicit argument
