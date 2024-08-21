def f[S: Monad](
  initial: S.Self = S.unit // error
) =
  S.unit // works