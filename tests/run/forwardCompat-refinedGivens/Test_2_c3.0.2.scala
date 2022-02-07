// Adapted from i12949

object Test:
  def main(args: Array[String]): Unit =
    summon[Catch22.TC["hi"]]
    summon[Catch22.TC[7.7]]
    summon[Catch22.TC[1]]
