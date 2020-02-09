def qc(using ctx: scala.quoted.QuoteContext) = println(ctx)
inline def g = qc  // error: no implicit argument
