trait Instrument {
  type R
  def result: R
}

trait InstrumentFactory[I <: Instrument] {
  def createInstrument: I
}

case class Instrumented[I <: Instrument](
  instrumentation: I#R)  // error

def instrumented[D, I <: Instrument](instrumentFactory: InstrumentFactory[I]): Instrumented[I] = {
  val instrument = instrumentFactory.createInstrument
  Instrumented(instrument.result)
}