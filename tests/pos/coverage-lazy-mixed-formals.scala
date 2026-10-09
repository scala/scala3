import language.experimental.captureChecking

def mixedValue(quotes: scala.quoted.Quotes): Unit =
  val marker = new Object
  def consume(m: marker.type, q: scala.quoted.Quotes): Unit = ()
  given scala.quoted.Quotes = quotes
  consume(marker, summon[scala.quoted.Quotes])
