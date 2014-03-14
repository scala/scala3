package dotty.runtime

/**
 * Classes used as holders for local lazy vals
 */
class LazyInt(init : =>Int) {
  lazy val value = init
}

class LazyLong(init : =>Long) {
  lazy val value = init
}

class LazyBoolean(init : =>Boolean) {
  lazy val value = init
}

class LazyDouble(init : =>Double) {
  lazy val value = init
}

class LazyFloat(init : =>Float) {
  lazy val value = init
}

class LazyByte(init : =>Byte) {
  lazy val value = init
}

class LazyRef(init : =>AnyRef) {
  lazy val value = init
}

class LazyShort(init : =>Short) {
  lazy val value = init
}

class LazyChar(init : =>Char) {
  lazy val value = init
}



