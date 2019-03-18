
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

import scala.MatchError;

@FunctionalInterface
public interface JFunction3<T1, T2, T3, R> extends scala.Function3<T1, T2, T3, R>, java.io.Serializable {
    default void $init$() {
    };
}
