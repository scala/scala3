trait F:
  type A

type G = (f: _ <: F) => f.A // error
