class CC
type Cap = {*} CC

def test(cap1: Cap, cap2: Cap) =
  var b: List[String => String] = Nil // was error, now OK
  val bc = b.head // error
