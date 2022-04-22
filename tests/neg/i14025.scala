val foo = summon[deriving.Mirror.Product { type MirroredType = [X] =>> [Y] =>> (X, Y) }] // error
val bar = summon[deriving.Mirror.Sum { type MirroredType = [X] =>> [Y] =>> List[(X, Y)] }] // error
