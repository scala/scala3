trait Cap { def use(): Unit }

def main() = {
  val f: (io: Cap^) -> () ->{} Unit =
    io => () => io.use()  // error

  val g: (Cap^) -> () ->{} Unit =
    io => () => io.use()  // error
}
