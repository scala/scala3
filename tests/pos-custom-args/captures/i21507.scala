import language.experimental.captureChecking

trait Box[cap Cap]:
  def store(f: (() -> Unit)^{Cap}): Unit

def run[cap Cap](f: Box[{Cap}]^{Cap} => Unit): Box[{Cap}]^{Cap} =
  new Box[{Cap}]:
    private var item: () ->{Cap} Unit = () => ()
    def store(f: () ->{Cap} Unit): Unit =
      item = f  // was error, now ok
