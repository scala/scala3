import language.experimental.captureChecking

def unlift[F[_], A, B](fab: F[A] =:= F[B]): A =:= B =
    type Invert[X] = X match
        case F[t] => t
    fab.liftCo[Invert]
