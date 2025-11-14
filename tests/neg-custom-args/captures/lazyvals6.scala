class IO extends caps.SharedCapability:
  def write(): Unit = ()

class C(val io: IO):
  lazy val x = () => io.write()
  val y: () ->{io} Unit = x
  val _: () -> Unit = x                 // error

  val c = C(io)
  lazy val l1 = () => c.io.write()
  val _: () ->{c.io} Unit = l1
  val _: () -> Unit = l1                // error

  lazy val l2 = () => this.io.write()
  val _: () ->{this.io} Unit = l2
  val _: () -> Unit = l2                // error

  val s1 = () => c.io.write()
  val _: () ->{c.io} Unit = s1
  val _: () -> Unit = s1                // error

  val s2 = () => this.io.write()
  val _: () ->{this.io} Unit = s2
  val _: () -> Unit = s2                // error

  lazy val d = C(io)
  lazy val l11 = () => d.io.write()
  val _: () ->{this} Unit = l11
      // it's just `this`, not `this.d`, since we also have to account for the fact
      // that `d` might be initialized here, so we have to charge its call captures
      // which are approximated by `this`.
  val _: () -> Unit = l11               // error

  lazy val l12 = () => this.io.write()
  val _: () ->{this.io} Unit = l12
  val _: () -> Unit = l12               // error

  val s11 = () => d.io.write()
  val _: () ->{this} Unit = s11
      // just `this`` not `this.d`, same reason as above
  val _: () -> Unit = s11               // error

  val s12 = () => this.io.write()
  val _: () ->{this.io} Unit = s1
  val _: () -> Unit = s12               // error

def test(io: IO) =
  lazy val x = () => io.write()
  val y: () ->{io} Unit = x
  val _: () -> Unit = x                 // error

  val c = C(io)
  lazy val z = () => c.io.write()
  val _: () -> {c.io} Unit = z
  val _: () -> Unit = z                 // error

