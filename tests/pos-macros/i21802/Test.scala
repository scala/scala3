//> using options -experimental -Ydebug

class ProbeFailedException(cause: Exception) extends Exception(cause)
trait Probing:
  self: Metrics =>
    val probeFailureCounter: MetricsGroup[Counter] =
      counters("ustats_probe_failures_count").labelled


trait Counter
class Metrics:
  class counters(name: String):
    transparent inline final def labelled: MetricsGroup[Counter] = MetricsGroup.refine[Counter]
