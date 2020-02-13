class Properties(val name: String) {

  private val props = new scala.collection.mutable.ListBuffer[(String,Any)]

  sealed class PropertySpecifier() {
    def update(propName: String, p: => Boolean) = {
      props += ((name+"."+propName, () => p))
    }
  }

  val property = new PropertySpecifier()
}

object PropSpecification extends Properties("Prop") {
  property("Prop.==> undecided") = true
}
