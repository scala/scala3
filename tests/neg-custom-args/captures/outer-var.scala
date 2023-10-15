class CC
type Cap = CC^

type Proc = () => Unit

def test(p: Proc) =
  var x: () ->{cap[test]} Unit = p
  var y = p

  def inner(q: Proc) =
    x = q           // error
    x = (q: Proc)   // error
    y = (q: Proc)   // error
    y = q           // error

  var finalizeActions = collection.mutable.ListBuffer[() => Unit]() // error


