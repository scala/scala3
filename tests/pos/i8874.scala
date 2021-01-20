trait Abc with
  opaque type Log = Double

class AbcClass extends Abc

val v = new AbcClass