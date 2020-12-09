
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcDJD$sp extends JFunction2<Object, Object, Object> {
    abstract double apply$mcDJD$sp(long v1, double v2);

    default Object apply(Object v1, Object v2) { return (Double) apply$mcDJD$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)); }
}
