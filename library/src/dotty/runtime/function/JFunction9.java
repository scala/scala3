
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R> extends scala.Function9<T1, T2, T3, T4, T5, T6, T7, T8, T9, R>, java.io.Serializable {
    default void $init$() {
    };

    @SuppressWarnings("unchecked")
    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, scala.Function1<T4, scala.Function1<T5, scala.Function1<T6, scala.Function1<T7, scala.Function1<T8, scala.Function1<T9, R>>>>>>>>> curried() {
        throw new UnsupportedOperationException("todo");
    }

    @SuppressWarnings("unchecked")
    default scala.Function1<scala.Tuple9<T1, T2, T3, T4, T5, T6, T7, T8, T9>, R> tupled() {
        throw new UnsupportedOperationException("todo");
    }


}
