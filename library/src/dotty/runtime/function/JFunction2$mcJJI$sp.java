
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcJJI$sp extends JFunction2<Object, Object, Object> {
    abstract long apply$mcJJI$sp(long v1, int v2);

    default Object apply(Object v1, Object v2) { return (Long) apply$mcJJI$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); }
}
