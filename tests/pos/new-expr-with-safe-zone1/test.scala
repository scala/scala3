import language.experimental.captureChecking
import scala.scalanative.safe.SafeZone

object SafeZoneTypeMatchTest{

  class A (v: Int = 0) {}

  // This test should fail because the type of sz should be `{*} SafeZone`.
  // It's a bug related to to cc but not safe zone.
  // TODO: fix issues/16686 and move this to neg tests.
  def test(): Unit = {
    val sz: SafeZone = SafeZone.open() // should be {*} SafeZone
    val a1 = new {sz} A(0)
  }
}