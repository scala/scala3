trait F:
  type A

type G = (f: ? <: F) => f.A // error
