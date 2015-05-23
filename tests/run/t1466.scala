object IOvervalueMyPrivacy {
  private[this] var i = 0
  def go = {
    List(1,2,3).foreach(i += _)
    i
  }
}

object Test extends dotty.runtime.LegacyApp {
  assert(IOvervalueMyPrivacy.go == 6)
}
