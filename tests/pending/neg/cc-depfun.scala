import language.experimental.captureChecking

// compare with neg-custom-args/captures/depfun.scala, which produces errors
// but the errors go away if ->{} gets replaced by ->.

trait Cap { def use(): Unit }

def main() = {
  val f: (io: Cap^) -> () -> Unit =
    io => () => io.use()  // error

  val g: (Cap^) -> () -> Unit =
    io => () => io.use()  // error
}
