trait Iterable { self =>
  //type CC <: Iterable { type CC = self.CC }
  type DD[X] <: Iterable { type DD[Y] = self.DD[Y] }
}
