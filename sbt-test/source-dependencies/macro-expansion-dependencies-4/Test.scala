package example

import com.softwaremill.macwire.*

object Test:
  try
    val d = wire[D0]
    val d1 = wire[D1]
    val d2 = wire[D2]
  catch
    case e: Throwable =>
      e.printStackTrace()
