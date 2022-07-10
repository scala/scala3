package deptest {
  @deprecated("Not used any more", since="7")
  object DeprecatedThing {
    val oldValue = 42
  }
}

package depuser {
  import deptest.DeprecatedThing.* // error

  object DepUser {
    def main(args: Array[String]): Unit = println {
      oldValue
    }
  }
}