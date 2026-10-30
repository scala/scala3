import language.experimental.captureChecking
import caps.ExclusiveCapability

class Ref extends ExclusiveCapability:
  def get: Int = 0

// Weird is not a classifier, so neither class is classified,
// and their fields need not have classified captures.
class Holder(r: Ref^) extends Weird:
  val f: Ref^{r} = r

class Holder2(r: Ref^) extends Weird, ExclusiveCapability:
  val f: Ref^{r} = r

