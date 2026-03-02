import compiletime.uninitialized
import caps.unsafe.untrackedCaptures

class LazyRef[T](val mkElem: () => T):
  @untrackedCaptures var elem: T = uninitialized
  @untrackedCaptures var evaluated = false
  def get: T =
    if !evaluated then
      elem = mkElem()
      evaluated = true
    elem
