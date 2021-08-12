import scala.language.implicitConversions

type Timeframe = "1m" | "2m" | "1H"
type TimeframeN = 1 | 2 | 60

def manualConvertToN(tf: Timeframe): TimeframeN = tf match
  case "1m" => 1
  case "2m" => 2
  case "1H" => 60
  case "4H" => ??? // was: no reachability warning

given Conversion[Timeframe, TimeframeN] =
  case "1m" => 1
  case "2m" => 2
  case "1H" => 60
  case "4H" => ??? // was: no reachability warning

given Conversion[TimeframeN, Timeframe] =
  case 1 => "1m"
  case 2 => "2m"
  case 60 => "1H"
  case 240 => ??? // was: no reachability warning
