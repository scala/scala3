import scala.annotation.valhalla

@valhalla
enum Option[+T] extends AnyVal:
  case Some(x: T)
  case Some1(x: T)
  case None

@valhalla
enum OptionDV[+T] extends AnyVal with DeepValhalla:
  case Some(x: T)
  case Some1(x: T)
  case None

class Main:
    def main = {

    }