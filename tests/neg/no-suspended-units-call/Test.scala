//> using options -Yno-suspended-units -Xcheck-macros

// The macro is defined in the same run, so it cannot be expanded. The error tree for it
// had no position, and -Xcheck-macros then crashed printing it.
def x: Int = three // error // error
