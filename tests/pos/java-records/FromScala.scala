object C:
  def useR1 =
    // constructor signature
    val r = R1(123, "hello")

    // accessors
    val i: Int = r.i
    val s: String = r.s

    // methods
    val iRes: Int = r.getInt()
    val sRes: String = r.getString()

    // supertype
    val record: java.lang.Record = r
