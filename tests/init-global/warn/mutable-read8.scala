object Stats {
  var monitored: Boolean = false
}

class UncachedGroundType {
  if (Stats.monitored) println("record stats") // warn
}

class LazyType extends UncachedGroundType

object NoCompleter extends LazyType