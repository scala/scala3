type Ext[S <: Seq[_]] = S match {
  case Seq[t] => t
}

def test = implicitly[Ext[Seq[Int]] =:= Int]
