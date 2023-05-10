class CC
type Cap = CC^

def test(cap1: Cap, cap2: Cap) =
  var b: List[String => String] = Nil // error
  val bc = b.head // was error, now OK
