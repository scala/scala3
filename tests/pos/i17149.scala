type Ext[S <: Seq[_]] = S match {
  case Seq[t] => t
}

val _ = implicitly[Ext[Seq[Int]] =:= Int] // e.scala: Cannot prove that e.Ext[Seq[Int]] =:= Int
val _ = summon[Ext[Seq[Int]] =:= Int]