//> using options -source future -deprecation -Xfatal-warnings

package deptest {
  @deprecated("Not used any more", since="7")
  object DeprecatedThing {
    val oldValue = 42
  }
}

package depuser {
  import deptest.DeprecatedThing.* // warn

  object DepUser {
    def main(args: Array[String]): Unit = println {
      oldValue
    }
  }
}

// nopos-error: No warnings can be incurred under -Werror.