def doIt0[E](error: => E): Unit = ()
inline def doIt[E](error: => E): Unit = doIt0(error)
