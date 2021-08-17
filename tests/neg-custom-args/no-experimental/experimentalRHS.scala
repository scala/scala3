import scala.annotation.experimental

@experimental
def x = ()

def d1 = x // error: value x is marked @experimental and therefore ...
@experimental def d2 = x

val v1 = x // error: value x is marked @experimental and therefore ...
@experimental val v2 = x

var vr1 = x // error: value x is marked @experimental and therefore ...
@experimental var vr2 = x

lazy val lv1 = x // error: value x is marked @experimental and therefore ...
@experimental lazy val lv2 = x
