import compiletime.uninitialized
class LazyRef[T](val mkElem: () => T):
  transparent var elem: T = uninitialized
  transparent var evaluated = false
  def get: T =
    if !evaluated then
      elem = mkElem()
      evaluated = true
    elem
