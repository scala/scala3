package config

import state._

object State {

  object Info {
    def apply(): Info = Info(Config(), Seq.empty)
  }
  case class Info(cfg: AnyRef, allTypes: Seq[AnyRef])
}
