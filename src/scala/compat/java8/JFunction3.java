
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction3<T1, T2, T3, R> extends scala.Function3<T1, T2, T3, R> {
    default void $init$() {
    };

    @SuppressWarnings("unchecked")
    default scala.Function1<T1, scala.Function1<T2, scala.Function1<T3, R>>> curried() {
      return scala.Function3$class.curried(this);
    }

    @SuppressWarnings("unchecked")
    default scala.Function1<scala.Tuple3<T1, T2, T3>, R> tupled() {
      return scala.Function3$class.tupled(this);
    }


}
