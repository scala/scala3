inline trait Vec[T: Specialized](val xs: List[T]):
    def method[S: Specialized](other: Vec[S]): Vec[(T, S)] // error: Only inline traits and inline functions may take specialized type parameters
