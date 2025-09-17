inline def simpleInlineWrap(f: => Any): Unit = f

@main def Test(): Unit = {
  simpleInlineWrap {
    object lifecycle {
      object Lifecycle {
        trait FromZIO
      }
    }
    object defn {
      val Lifecycle: lifecycle.Lifecycle.type = lifecycle.Lifecycle
    }
    val xa: defn.Lifecycle.type = defn.Lifecycle
  }

  // more nested case
  simpleInlineWrap {
    object lifecycle {
      object Lifecycle {
        object FromZIO
      }
    }
    object defn {
      val Lifecycle: lifecycle.Lifecycle.type = lifecycle.Lifecycle
    }
    val xa: defn.Lifecycle.FromZIO.type = defn.Lifecycle.FromZIO
  }
}
