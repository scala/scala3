package cps

val c = async[ComputationBound] {
  List(1,2,3,4).collectFirst { case x if x > 0 => x > 3 }
}
