type F[_] = A
type A = F[?]  // error: cyclic
