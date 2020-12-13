package inlinedefs

object FakePredef:

  /** Super long padded documentation
   *  Lorem ipsum dolor sit amet, consectetur adipiscing elit,
   *  sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
   *  Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
   *  nisi ut aliquip ex ea commodo consequat.
   *  Duis aute irure dolor in reprehenderit in voluptate velit esse cillum
   *  dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident,
   *  sunt in culpa qui officia deserunt mollit anim id est laborum.
   */
  transparent inline final def assert(inline assertion: Boolean): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }
