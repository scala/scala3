package scala.quoted

def qctx(using qctx: QuoteContext): qctx.type = qctx
