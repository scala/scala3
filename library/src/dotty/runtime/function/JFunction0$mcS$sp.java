
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction0$mcS$sp extends JFunction0 {
    abstract short apply$mcS$sp();

    default Object apply() { return (Short) apply$mcS$sp(); }
}
