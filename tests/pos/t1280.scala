trait X { x => type T >: Null; new X { type T = Any & x.T } }
