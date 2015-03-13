case class C(x: Int, var y: String) {

}


class Top {

  final override def hashCode: Int = 2

}

case class Sub() extends Top
