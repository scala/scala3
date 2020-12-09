
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcDJI$sp extends JFunction2<Object, Object, Object> {
    abstract double apply$mcDJI$sp(long v1, int v2);

    default Object apply(Object v1, Object v2) { return (Double) apply$mcDJI$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); }
}
