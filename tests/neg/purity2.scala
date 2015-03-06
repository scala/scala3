import dotty.pure

object purity2 {

  class C

  def A = {
    var ref1 = new C

    @pure def B = {
      var ref2 = new C

      ref1 = ref2   // ERROR 1 ReimPhase
//      ref2 = ref1   // ERROR 2 ReimTyper
    }
  }
}
