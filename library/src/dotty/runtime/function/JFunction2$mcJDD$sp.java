
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcJDD$sp extends JFunction2<Object, Object, Object> {
    abstract long apply$mcJDD$sp(double v1, double v2);

    default Object apply(Object v1, Object v2) { return (Long) apply$mcJDD$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToDouble(v2)); }
}
