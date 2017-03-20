class A(val member: Int) {
  def getAMember = member
}

class SubA(member: Int) extends A(member) {
  def getSubAMember = member
}
