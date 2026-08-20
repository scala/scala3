class Cap extends caps.SharedCapability

def test(capEval: Cap, capFun: Cap, capBad: Cap) =
  def accept(op: ->{capEval, capFun} (Int ->{capFun} Int)) = op
  accept {
    if capEval == capEval then ()
    (x: Int) =>
      capBad // error
      x
  }
