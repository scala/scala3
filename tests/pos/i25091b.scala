object Test:
  trait VersionComponent[T]:
      inline def minimum: T = wrap(42)
      inline def wrap(value: Int): T

  opaque type MajorVersionType = Int
  object MajorVersion extends VersionComponent[MajorVersionType]:
      inline def wrap(value: Int): MajorVersionType = value
      inline def reset: MajorVersionType = minimum

  trait Increment[F]:
    def increment: MajorVersionType

  class MyIncrement extends Increment[MajorVersionType]:
    inline def increment: MajorVersionType = MajorVersion.reset
