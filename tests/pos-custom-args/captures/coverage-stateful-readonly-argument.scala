import language.experimental.captureChecking
import caps.{Stateful, any}

final class Cell extends Stateful

def fresh(): Cell^ = new Cell
def observe(cell: Cell^{any.rd}): Unit = ()

def test(): Unit =
  observe(fresh())
