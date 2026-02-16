type outer = Int
object outer:
  type DFBits = Long
  object DFBits:
    def a: Unit = f // forces suspension of this compilation unit in typer
    def b: DFVal = 2 // uses implicit conversion `DFVal.conv`

    trait Candidate
    object Candidate:
      given candidate: Candidate = ??? // completed in run 3 but created in run 2
  end DFBits

  trait DFVal
  object DFVal extends Conversions
