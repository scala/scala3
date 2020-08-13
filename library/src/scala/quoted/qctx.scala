package scala.quoted

/** Current QuoteContext in scope */
def qctx(using qctx: QuoteContext): qctx.type = qctx
