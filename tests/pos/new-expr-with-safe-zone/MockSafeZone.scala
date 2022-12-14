package scala.scalanative.safe

trait SafeZone(zoneHandle: String) {
  override def toString(): String = s"SafeZone{$zoneHandle}"
}

class MemorySafeZone(zoneHandle: String) extends SafeZone(zoneHandle) {
  override def toString(): String = s"MemorySafeZone($zoneHandle)"
}
