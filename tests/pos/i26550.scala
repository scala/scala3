class BitNumWrapper(val value: 0 | 1) extends AnyVal
def positive(w: BitNumWrapper): Boolean = w.value > 0
