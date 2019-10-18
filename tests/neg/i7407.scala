def qc(given ctx: scala.quoted.QuoteContext) = println(ctx)
inline def g = qc  // error: cyclic reference
