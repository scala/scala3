// scalac: -Werror -Xlint:longLit,deprecation -deprecation

trait DejectedLiterals:

  def bad = 1l        // error

  def worse = 123l    // error

  def worstest = 32l  // error

// Lowercase el for long is not recommended because it is easy to confuse with numeral 1; use uppercase L instead
