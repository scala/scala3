enum Nums { case One; case Two }

def fok(that: Nums) = ???
val ok = fok(Nums.One)

inline def fko(inline that: Nums) = inline that match {
  case Nums.One => "fff(one)"
}
val ko = fko(Nums.One)
