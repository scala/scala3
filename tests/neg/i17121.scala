class Consumer[-T]

object Test:
  type F1[X] = X match { case List[t] => t } // OK
  summon[F1[List[?]] =:= Any]

  type F2[X] = X match { case Consumer[t] => t } // OK
  summon[F2[Consumer[?]] =:= Nothing]

  type F3[X] = X match { case List[Consumer[t]] => t } // OK
  summon[F3[List[Consumer[?]]] =:= Nothing]

  type G1[X] = X match { case Consumer[List[t]] => t } // error

  type G2[X] = X match { case Consumer[Consumer[t]] => t } // error

  type G3[X] = X match { case Consumer[Consumer[Consumer[t]]] => t } // error

  type G4[X] = X match { case Consumer[List[Consumer[t]]] => t } // error
end Test
