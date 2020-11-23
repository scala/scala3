package scala.quoted

/** Current Quotes in scope */
def qctx(using q: Quotes): q.type = q
