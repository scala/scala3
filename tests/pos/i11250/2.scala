package shapeless3.data

object MonoidalTest2 { // But not here
  type p = (Int, (String, (Boolean, Unit)))
  summon[pairs.length[p] =:= 3]
}

