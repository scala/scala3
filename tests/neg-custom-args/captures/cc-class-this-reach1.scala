import language.experimental.captureChecking
import caps.*
trait Runner:
  def run: () ->{this} Unit
class Runner1(f: List[() => Unit]) extends Runner:
  def run: () ->{f*} Unit = f.head // error
class Runner2(@use f: List[() => Unit]) extends Runner:
  def run: () ->{f*} Unit = f.head // ok
