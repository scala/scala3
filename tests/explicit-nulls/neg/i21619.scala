class SomeException extends Exception

def test1: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: Exception =>
  x.replace("", "") // error

def test2: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: NoSuchMethodError =>
      x = "e"
  x.replace("", "") // ok

// From i24296
def test2_2: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: Exception =>
      x = "e"
    case _ =>
  x.replace("", "") // error

def test2_3: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: NoSuchMethodError =>
      x = "e"
    case e: AbstractMethodError =>
      x = "e"
  x.replace("", "") // ok

def test2_4: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: NoSuchMethodError =>
      x = "e"
    case e: AbstractMethodError =>
      throw new Exception()
  x.replace("", "") // ok

def test3: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case e: Exception =>
  finally
    x = "f"
  x.replace("", "") // ok

def test4: String =
  var x: String | Null = null
  x = ""
  var i: Int = 1
  try
    try
      if i == 1 then
        x = null
        throw new Exception()
      else
        x = ""
    catch
      case _ =>
    x = ""
  catch
    case _ =>
  x.replace("", "") // error

def test5: Unit =
  var x: String | Null = null
  var y: String | Null = null
  x = ""
  y = ""
  var i: Int = 1
  try
    i match
      case _ =>
        x = null
        throw new Exception()
    x = ""
  catch
    case _ =>
  val z1: String = x.replace("", "") // error
  val z2: String = y.replace("", "")

def test6 = {
  var x: String | Null = ""
  var y: String = ""
  x = ""
  y = if (false) x else 1 match {
    case _ => {
      x = null
      y
    }
  }
  x.replace("", "") // error
}

// From i24296
def test7() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e =>
      throw e
  }
  x.trim() // ok

def test8() =
  var x: String | Null = null
  try {
    try {
      x = ""
    } catch {
      case e => throw e
    }
  } catch {
    case e => throw e
  }
  x.trim() // ok

def test9() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e: AssertionError =>
      throw e
    case _ =>
  }
  x.trim() // error

def test9_2() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e: AssertionError =>
      throw e
  }
  x.trim() // ok

def test10() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e =>
      throw e
  } finally {
    x = null
  }
  x.trim() // error

def test11() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e =>
      x = null
      throw e
  } finally {
    x = ""
  }
  x.trim() // ok

def test12() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e =>
      x = null
      throw e
  } finally {
    throw new Exception
    x = ""
  }
  x.trim() // ok

def test12_2() =
  var x: String | Null = null
  try {
    x = ""
  } catch {
    case e =>
      x = null
      throw e
  } finally {
    throw new Exception
  }
  x.trim() // ok

def test13() =
  var x: String | Null = null
  try {
    x = null
    throw new RuntimeException
  } finally {
    x.trim() // error
  }
  x.trim() // OK

def test14() =
  var x: String | Null = ""
  x = ""
  try {
    try {
    } catch {
      case e =>
        x = null
        throw e
    }
  } catch {
    case e =>
      x.trim() // error
  }



def test15: String =
  var x: String | Null = ???
  var y: String | Null = ???
  // ...
  try
    x = null
    // ...
    x = ""
  catch
    case e: SomeException =>
      x = null
      // situation 1: don't throw or return
      // situation 2:
      return ""
  finally
    y = x
    // should always error on y.trim

  y.trim  // error (ideally, should error if situation 1, should not error if situation 2)

def test16: String =
  var x: String | Null = ???
  x = ""
  try
    // call some method that throws
    // ...
    x = ""
  catch
    case e: SomeException =>
      x = null
      // call some method that throws
      // ...
      x = "<error>"
  finally {}

  x.trim() // ok

def test17: String =
  var x: String | Null = ???
  x = ""
  try
    // call some method that throws
    // ...
    x = ""
  catch
    case e: SomeException =>
      x = null
      // call some method that throws
      // ...
      x = "<error>"
  finally
    println(x.trim()) // error

  ""

def test18: String =
  var x: String | Null = ???
  var y: String | Null = ???
  // ...
  try
    x = null
    y = null
    // ...
    x = ""
    y = ""
  catch
    case e: SomeException =>
      x = null
      return ""
  finally {}

  x.trim + y.trim


def test19: String =
  var x: String | Null = ???
  try
    x = null
  catch
    case e: SomeException =>
      x = null
      throw e
  finally
    throw new Exception()
  x.trim // ok

def test20: String =
  var x: String | Null = ???
  x = ""
  try
    x = ""
  catch
    case e: SomeException =>
      x = ""
  finally {}
  x.trim // ok