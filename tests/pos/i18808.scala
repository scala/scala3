//> using options -Werror

import language.future

type F[X] = X match
  case List[_] => Int

type G[X] = X match
  case List[?] => Int
