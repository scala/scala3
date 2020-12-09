
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcZJJ$sp extends JFunction2<Object, Object, Object> {
    abstract boolean apply$mcZJJ$sp(long v1, long v2);

    default Object apply(Object v1, Object v2) { return (Boolean) apply$mcZJJ$sp(scala.runtime.BoxesRunTime.unboxToLong(v1), scala.runtime.BoxesRunTime.unboxToLong(v2)); }
}
