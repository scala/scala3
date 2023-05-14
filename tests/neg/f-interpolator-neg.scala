object Test {

  def numberArgumentsTests(s : String, d : Int) = {
    new StringContext().f() // error
    new StringContext("", " is ", "%2d years old").f(s) // error
    new StringContext("", " is ", "%2d years old").f(s, d, d) // error
    new StringContext("", "").f() // error
  }

  def interpolationMismatches(s : String, f : Double, b : Boolean) = {
    f"$s%b" // error
    f"$s%c" // error
    f"$f%c" // error
    f"$s%x" // error
    f"$b%d" // error
    f"$s%d" // error
    f"$f%o" // error
    f"$s%e" // error
    f"$b%f" // error
    f"$s%i" // error
  }

  def flagMismatches(s : String, c : Char, d : Int, f : Double, t : java.util.Date) = {
    f"$s%+ 0,(s" // error
    f"$c%#+ 0,(c" // error
    f"$d%#d" // error
    f"$d%,x" // error
    f"$d%+ (x" // error
    f"$f%,(a" // error
    f"$t%#+ 0,(tT" // error
    f"%-#+ 0,(n" // error
    f"%#+ 0,(%" // error
  }

  def badPrecisions(c : Char, d : Int, f : Double, t : java.util.Date) = {
    f"$c%.2c" // error
    f"$d%.2d" // error
    f"%.2%" // error
    f"%.2n" // error
    f"$f%.2a" // error
    f"$t%.2tT" // error
  }

  def badIndexes() = {
    f"%<s" // error
    f"%<c" // error
    f"%<tT" // error
    f"${8}%d ${9}%d %3$$d" // error
    f"${8}%d ${9}%d%0$$d" // error
  }

  def warnings(s : String) = {
    f"${8}%d ${9}%1$$d"
    f"$s%s $s%s %1$$<s"
    f"$s%s $s%1$$s"
  }

  def badArgTypes(s : String) = {
    f"$s%#s" // error
  }

  def misunderstoodConversions(t : java.util.Date, s : String) = {
    f"$t%tG" // error
    f"$t%t" // error
    f"$s%10.5" // error
  }

  def otherBrainFailures(d : Int) = {
    f"${d}random-leading-junk%d" // error
    f"%1$$n"
    f"%1$$d" // error
    f"blablablabla %% %.2d" // error
    f"blablablabla %.2b %%" // error

    f"ana${3}%.2f%2${true}%bb" // error
    f"ac{2c{2{c.ca "

    f"b%c.%2ii%iin" // error // error // error
    f"b}22%2.c<{%{"  // error // error
    f"%%bci.2${'i'}%..2c2" // error
  }

}
