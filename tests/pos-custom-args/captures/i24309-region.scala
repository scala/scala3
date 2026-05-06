import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.*
import caps.unsafe.untrackedCaptures

object Regions:
  class Ref(@untrackedCaptures private var x: Int):
    def get = x
    def set(y: Int) = x = y

  trait Region[R^] extends SharedCapability:
    region: Region[R]^ =>
      def alloc(value: Int): Ref^{R} = Ref(value)

      def subregion[T](f: [R2^ >: R] => (Region[R2]) => T): T =
        val r = new Region[R] {}
        f(r)


  object Region:
    def apply[T](f: [R^] => Region[R] => T): T =
      val r = new Region[{}] {}
      f(r)

  @main def main() =
    import Region.*
    Region: [R^] =>
     r1 =>
      val x = r1.subregion: [R2^ >: R] =>
       r2 =>
        val a = r1.alloc(0)
        val b = r2.alloc(0)
        a
      0
