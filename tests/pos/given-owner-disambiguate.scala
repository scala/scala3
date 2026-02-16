class General
class Specific extends General

class LowPriority:
  given a:General()

object NormalPriority extends LowPriority:
  given b:Specific()

def run =
  import NormalPriority.given
  val x = summon[General]
  val _: Specific = x // <- b was picked