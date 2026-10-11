import language.experimental.captureChecking
import caps.Control

// "Classifying Capabilities" §6.5: a stored Failure holds the CanThrow for a captured
// exception. CanThrow is a Control capability, so the result advertises f.only[Control].
class CanThrow[-E <: Exception] extends Control   // stdlib's is `extends caps.Control`

case class Failure(e: Exception, ct: CanThrow[e.type]^)

def captureException(f: () => Unit): Option[Failure^{f.only[Control]}] = ???
