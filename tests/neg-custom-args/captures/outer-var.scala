class CC
type Cap = CC^

type Proc = () => Unit

def test(p: Proc, q: () => Unit) =
  var x: () ->{p, q} Unit = p
  var y = p // OK, y has type () ->{p} Proc

  def inner(q: Proc) =
    x = q           // error
    x = (q: Proc)   // error
    y = (q: Proc)   // error
    y = q           // error

  var finalizeActions = collection.mutable.ListBuffer[() => Unit]() // error


