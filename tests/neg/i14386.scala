// scalac: -Werror
def logLevelDetail(level: Int): String =
   s"""$level

// the following line is indented using [tab][tab]
		Sets the global logging level to $level.
    """
 /* was 2 errors with carets as shown
  |                                               ^    ^
  |                                               Incompatible combinations of tabs and spaces in indentation prefixes.
  |                                               Previous indent : 3 spaces
  |                                               Latest indent   : 2 tabs
  */

def log(level: Int, msg: String): String =
  s"""
		$level
  prefixed $level suffixed
  """
/*
            ^    ^
            Incompatible combinations of tabs and spaces in indentation prefixes.
            Previous indent : 2 tabs
            Latest indent   : 2 space
 */

// normal mixed tabs errors as a baseline
def g =
    42
	+ 17   // error

def p() =
    println("hello")
	println("world")  // error
  /*
   |    Incompatible combinations of tabs and spaces in indentation prefixes.
   |    Previous indent : 4 spaces
   |    Latest indent   : 1 tab
   */

def braced() =
  s"""begin
  ${
      val level = 10
    val msg = "hello, world"  // error he lets me off with a warning
    log(level, msg)           // error
   }
   end"""
