
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

import scala.MatchError;

@FunctionalInterface
public interface JFunction6<T1, T2, T3, T4, T5, T6, R> extends scala.Function6<T1, T2, T3, T4, T5, T6, R>, java.io.Serializable {
    default void $init$() {
    };
}
