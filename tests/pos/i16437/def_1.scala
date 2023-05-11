class OU {
  def dn: String = ""
}

class Inventory() {
  object NODES extends OU { nodes =>
    val x = nodes.dn
  }
}
