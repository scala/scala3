import language.experimental.captureChecking
import language.experimental.erasedDefinitions
import caps.{Classifier, SharedCapability}

// "Classifying Capabilities" §6.3: an encapsulated File whose operations are typed with
// `this`-projections, so read needs only the Read part and write only the ReadWrite part.
trait Read      extends Classifier, SharedCapability
trait ReadWrite extends Classifier, SharedCapability

trait File(val name: String) extends SharedCapability:
  erased val r:  Read^        // paper marks these private; abstract members cannot be
  erased val rw: ReadWrite^
  val read:  () ->{this.only[Read]}      Int
  val write: Int ->{this.only[ReadWrite]} Unit

def test(f: File^): Unit =
  val ro:   Object^{f.only[Read]}  = ???
  val full: Object^{f}             = ro    // {f.only[Read]} <: {f}
  val rd:   () ->{f.only[Read]} Int = f.read
  val rw:   Object^{f.only[ReadWrite]} = ???
  val viaExc: Object^{f.except[Read]}  = rw   // Read and ReadWrite are disjoint
