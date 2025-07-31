
//> using options -Wunused:all

package p1:
  import java.lang.Runnable
  import java.lang.{Thread, String}, java.lang.Integer
  class C extends Runnable { def run() = () }

package p2:
  import java.lang.Thread
  import java.lang.String
  import java.lang.Runnable
  import java.lang.Integer
  class C extends Runnable { def run() = () }

package p3:
  import java.lang.{Runnable, Thread, String}
  class C extends Runnable { def run() = () }

package p4:
  import java.lang.{Runnable, System, Thread}, System.out
  class C extends Runnable { def run() = out.println() }

package p5:
  import java.lang.{Thread, Runnable, System}, System.out
  class C extends Runnable { def run() = out.println() }

package p6:
  import java.lang.{Runnable, System}, java.lang.Thread, System.out
  class C extends Runnable { def run() = out.println() }

package p7:
  import java.lang.{Runnable,
                    Thread,
                    System}, System.out
  class C extends Runnable { def run() = out.println() }

package p8:
  import java.lang.{Runnable as R, System,
                    Thread}, System.out
  class C extends R { def run() = out.println() }

package p9:
  import java.lang.{Runnable as R, System,
                    Thread}, System.out
  class C extends R { def run() = Thread(() => System.out.println()).start() }

package p10:
  object X:
    def x = 42
  class C:
    import X.*, java.util.HashMap // preserve text, don't rewrite to p10.X.*
    def c = x

package p11:
  import collection.mutable, mutable.ListBuffer, java.lang.{Runnable, Thread} // warn // warn // not checked :(
  def buf = ListBuffer.empty[String]

package p12:
  import collection.mutable, java.lang.System, java.lang.Runnable
  class C extends Runnable { def run() = System.out.println() }

package p13:
  import java.lang.{Runnable,

                    Thread, // leave one blank line instead of two

                    System}, System.out
  class C extends Runnable { def run() = out.println() }

package p14:
  import collection.mutable

  import java.lang.{Runnable, Thread}

  import mutable.ListBuffer

  def buf = ListBuffer.empty[String]
