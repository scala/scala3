import language.experimental.captureChecking
import caps.unsafe.untrackedCaptures

trait Box[Cap^]:
  def store(f: (() -> Unit)^{Cap}): Unit

def run[Cap^](f: Box[{Cap}]^{Cap} => Unit): Box[{Cap}]^{Cap} =
  new Box[{Cap}] {
    @untrackedCaptures private var item: () ->{Cap} Unit = () => ()
    def store(f: () ->{Cap} Unit): Unit =
      item = f  // was error, now ok
  }
