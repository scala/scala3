
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction0$mcI$sp extends JFunction0 {
    abstract int apply$mcI$sp();

    default Object apply() { return (Integer) apply$mcI$sp(); }
}
