trait Chain { type Tail <: Chain }

extension [C1 >: Chain <: Chain](c2: c1.Tail) // error
  def ra1_:[C2 <: C1](c1: C1): C2 = ???

extension [C1](c2: (C1, C2)) // error
  def ra2_:[C2 <: C1](c1: (C1, C2)): C2 = ???

extension [C1](c2: C2) // error
  def ra3_:[C2 <: C1](c1: C1): C2 = ???

extension [C1](c2: (C1, C2, C3)) // error // error
  def ra4_:[C2 <: C1, C3 <: C1](c1: (C1, C2)): C2 = ???

extension [C1](str: String)(using z: (str.type, C2)) // error
  def ra5_:[C2 <: Int](c1: C1): C2 = ???

type D2 = String
extension [D1 <: Int](D2: (D1, D2)) // error
  def sa2_:[D2 <: D1](D1: (D1, D2)): D2 = ???
