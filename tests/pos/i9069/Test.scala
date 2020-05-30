def Test =
  val mirror = summon[deriving.Mirror.SumOf[Foo]]
  summon[mirror.MirroredElemTypes =:= (Foo.Baz.type, Bar)]
