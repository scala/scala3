import caps.*

class IO extends SharedCapability:
  def println(): Unit = ()

class Ref extends Mutable:
  var x = 0
  update def inc(): Unit = x += 1

abstract class PureSAM extends Pure:
  def apply(): Unit

trait PureSAMTrait extends Pure:
  def apply(): Unit

abstract class Reader extends Stateful:
  def apply(): Unit

trait ReaderTrait extends Stateful:
  def apply(): Unit

def testPure1(io: IO): PureSAM = () => io.println() // error
def testPure2(io: IO): PureSAMTrait = () => io.println() // error

def testStateful1(r: Ref^) =
  val f: Reader^ = () => r.inc() // error
  val ro: Reader^{f.rd} = f
  ro.apply()

def testStateful2(r: Ref^) =
  val f: ReaderTrait^ = () => r.inc() // error
  val ro: ReaderTrait^{f.rd} = f
  ro.apply()
