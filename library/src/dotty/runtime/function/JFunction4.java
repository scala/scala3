
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

import scala.MatchError;

@FunctionalInterface
public interface JFunction4<T1, T2, T3, T4, R> extends scala.Function4<T1, T2, T3, T4, R>, java.io.Serializable {
    default void $init$() {
    };

    @SuppressWarnings("unchecked")
    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, scala.Function1<T4, R>>>> curried() {
      return x1 -> x2 -> x3 -> x4 -> apply(x1, x2, x3, x4);
    }

    @SuppressWarnings("unchecked")
    default scala.Function1<scala.Tuple4<T1, T2, T3, T4>, R> tupled() {
        return x0$1 -> {
            if (x0$1 == null) {
                throw new MatchError(x0$1);
            }
            T1 x1 = x0$1._1();
            T2 x2 = x0$1._2();
            T3 x3 = x0$1._3();
            T4 x4 = x0$1._4();
            R r = this.apply(x1, x2, x3, x4);
            return r;
        }
        ;
    }


}
