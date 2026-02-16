transparent trait VersionComponent[T]:
    protected def minimumValue: Int
    inline def minimum: T = wrap(minimumValue)
    inline def reset: T = minimum
    inline def wrap(value: Int): T
    inline def unwrap(value: T): Int

opaque type MajorVersion = Int
object MajorVersion extends  VersionComponent[MajorVersion]:
    protected inline def minimumValue: Int = 0
    inline def wrap(value: Int): MajorVersion = value
    inline def unwrap(mv: MajorVersion): Int = mv

case class Version(major: MajorVersion)
object Version:
  trait Increment[F]:
    extension (v: Version) def increment: Version

  object Increment:
    given Increment[MajorVersion]:
      extension (v: Version)
        inline def increment: Version = Version(MajorVersion.reset)
