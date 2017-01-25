class Test[X](x: X) {
  def checkSpecialization[Y](y: Y): X = {
    def specMe[@specialized T]() = ()
    x
  }
  private def checkNameStartsWith(prefix: String) = { (new Exception) }
}
