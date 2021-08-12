package inlinedefs

object FakePredef/*<-inlinedefs::FakePredef.*/:

  /** Super long padded documentation
   *  Lorem ipsum dolor sit amet, consectetur adipiscing elit,
   *  sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
   *  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
   *  nisi ut aliquip ex ea commodo consequat.
   *  Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
   *  dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
   *  sunt in culpa qui officia deserunt mollit anim id est laborum.
   */
  transparent inline final def assert/*<-inlinedefs::FakePredef.assert().*/(inline assertion/*<-inlinedefs::FakePredef.assert().(assertion)*/: Boolean/*->scala::Boolean#*/): Unit/*->scala::Unit#*/ = {
    if (!assertion/*->inlinedefs::FakePredef.assert().(assertion)*/)
      throw new java.lang.AssertionError/*->java::lang::AssertionError#*/("assertion failed")
  }
