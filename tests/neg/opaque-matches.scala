object NT:
  opaque type NT[A, B] = B

  type Fst[X] = X match   // error
    case NT[a, b] => a

  inline def fst[NT](nt: NT): Fst[NT] =
    ???

  inline def snd[NT](nt: NT): NTDecomposition.Snd[NT] = // error
    ???

object NTDecomposition:

  type Snd[X] = X match
    case NT.NT[a, b] => b



