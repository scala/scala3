
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcJDJ$sp extends JFunction2<Object, Object, Object> {
    abstract long apply$mcJDJ$sp(double v1, long v2);

    default Object apply(Object v1, Object v2) { return (Long) apply$mcJDJ$sp(scala.runtime.BoxesRunTime.unboxToDouble(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); }
}
