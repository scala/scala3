enum HLst {
  case HCons[+Hd, +Tl <: HLst](hd: Hd, tl: Tl)
  case HNil
}

object Test {
  import HLst.*
  def length(hl: HLst): Int = hl match {
    case HCons(_, tl) => 1 + length(tl)
    case HNil => 0
  }
  def sumInts(hl: HLst): Int = hl match {
    case HCons(x: Int, tl) => x + sumInts(tl)
    case HCons(_, tl) => sumInts(tl)
    case HNil => 0
  }
  def main(args: Array[String]) = {
    val hl = HCons(1, HCons("A", HNil))
    assert(length(hl) == 2, length(hl))
    assert(sumInts(hl) == 1)
  }
}
