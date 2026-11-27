import caps.*

class IO extends SharedCapability

def test1(io: IO, xs: Seq[() ->{io} Unit]): Seq[() -> Unit] = xs match
  case Seq(_, rest*) => rest // error
  case _ => Nil

def test2(io: IO, xs: Seq[() ->{io} Unit]): Seq[() -> Unit] = xs match
  case Seq(_, rest @ _*) => rest // error
  case _ => Nil
