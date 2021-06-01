enum ErrorMessageID extends java.lang.Enum[ErrorMessageID] {
    case
        LazyErrorId, // // errorNumber: -2
        NoExplanationID
    def errorNumber = ordinal - 2
}
