// contributed by Lauri Alanko
trait TypeSub {
  type l
  type u
  def castSub[f[+x]](fl : f[l]) : f[u]
  def castSuper[f[-x]](fu : f[u]) : f[l] = {
    type c[+y] = f[y] => f[l]
    castSub[c]{ (fl : f[l]) => fl }(fu)
  }
  def castValue[t](lt : l & t) : u & t = {
    type c[+y] = y & t
    castSub[c](lt)
  }
}
