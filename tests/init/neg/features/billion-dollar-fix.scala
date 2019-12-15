import scala.annotation._

object Bilion {
  class Security(_dba: => DBA) {
    def work(): Unit = ???
  }
  class DBA(_s: => Security, _gui: => GUI) {
    def work(): Unit = ???
  }
  class SMS(_s: => Security, _dba: => DBA) {
    def work(): Unit = ???
  }
  class GUI(_s: => Security, _sms: => SMS, _dba: => DBA) {
    def work(): Unit = ???
  }
}

import Bilion._

final class Billion1 {
  val s: Security = new Security(dba)
  val dba: DBA = new DBA(s, gui)

  s.work()
  dba.work()

  val sms: SMS = new SMS(s, dba)
  val gui: GUI = new GUI(s, sms, dba)

  s.work()    // ok
  dba.work()  // ok
  sms.work()  // ok
  gui.work()  // ok
}
