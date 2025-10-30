class foo(elem: Int with elem > 0)

class Multi(x: Int with x > 0)(y: String with y.length > 2, z: Double with z >= 0.0)

case class MultiCase(x: Int with x > 0)(val y: String with y.length > 2, val z: Double with z >= 0.0)

@main def Test = ()
