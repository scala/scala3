enum View[+T]:
  case Refl(f: T => T) // error: covariant T in contravariant position... enum case may require explicit type parameters
