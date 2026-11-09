import language.experimental.safe
def assertPure(op: () -> Unit): Unit = ()
def testQueue(): Unit =
  val q = new java.util.concurrent.ConcurrentLinkedQueue[String]() // error
  assertPure: () =>
    q.add("secret")

def testArrayList(): Unit =
  val xs = new java.util.ArrayList[String]()  // error
  assertPure: () =>
    xs.add("secret")

def testHashMap(): Unit =
  val m = new java.util.HashMap[String, String]() // error
  assertPure: () =>
    m.put("leak", "secret")

def testArrayDeque(): Unit =
  val dq = new java.util.ArrayDeque[String]() // error
  assertPure: () =>
    dq.addLast("secret")