object O:
  opaque type T = Int

  inline def get0: Int = Do.get              // no proxy needed
  inline def get1: Int = Do.get: O.T         // no proxy needed
  inline def get2: Int = Do.get:   T         // proxied

  inline def set0: Unit = Do.set(0)          // was: broken
  inline def set1: Unit = Do.set(1: O.T)     // no proxy needed
  inline def set2: Unit = Do.set(2:   T)     // proxied

  inline def mod0: Int = Do.mod(0)           // was: broken
  inline def mod1: Int = Do.mod(1): O.T      // was: broken
  inline def mod2: Int = Do.mod(2):   T      // was: broken
  inline def mod3: Int = Do.mod(3: O.T)      // no proxy needed
  inline def mod4: Int = Do.mod(4: O.T): O.T // no proxy needed
  inline def mod5: Int = Do.mod(5: O.T):   T // proxied
  inline def mod6: Int = Do.mod(6:   T)      // proxied
  inline def mod7: Int = Do.mod(7:   T): O.T // proxied
  inline def mod8: Int = Do.mod(8:   T):   T // proxied

class Test:
  def testGet0: Int = O.get0
  def testGet1: Int = O.get1
  def testGet2: Int = O.get2

  def testSet0: Unit = O.set0
  def testSet1: Unit = O.set1
  def testSet2: Unit = O.set2

  def testMod0: Int = O.mod0
  def testMod1: Int = O.mod1
  def testMod2: Int = O.mod2
  def testMod3: Int = O.mod3
  def testMod4: Int = O.mod4
  def testMod5: Int = O.mod5
  def testMod6: Int = O.mod6
  def testMod7: Int = O.mod7
  def testMod8: Int = O.mod8

object Do:
  def get: O.T          = ???
  def set(x: O.T): Unit = ()
  def mod(x: O.T): O.T  = x
