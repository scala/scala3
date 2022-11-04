enum ErrorMessageID extends java.lang.Enum[ErrorMessageID] {
    case
        LazyErrorId,
        NoExplanationID // error
    def errorNumber = ordinal - 2
}
