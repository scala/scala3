package test
import language.experimental.safe
import caps.unsafe.untrackedCaptures
import scala.annotation.unchecked.{uncheckedCaptures, uncheckedVariance}
import scala.util.{Random, Properties}

object Test:

  if Properties.propIsSet("foo") then
    Properties.clearProp("foo") // error
    Properties.setProp("foo", "invalid") // error

  Properties.main(Array()) // error

