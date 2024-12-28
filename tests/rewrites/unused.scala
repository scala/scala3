
//> using options -Wunused:all

package p1:
  import java.lang.Runnable
  import java.lang.Thread
  class C extends Runnable { def run() = () }
