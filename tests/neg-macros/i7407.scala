def qc(using ctx: scala.quoted.Quotes) = println(ctx)
inline def g = qc  // error: no implicit argument
