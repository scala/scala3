type T[A] = A | Any

def perform[A](using T[A]): A = perform2

def perform2[A](using T[A]): A = ???