
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction2$mcDII$sp extends JFunction2<Object, Object, Object> {
    abstract double apply$mcDII$sp(int v1, int v2);

    default Object apply(Object v1, Object v2) { return (Double) apply$mcDII$sp(scala.runtime.BoxesRunTime.unboxToInt(v1), scala.runtime.BoxesRunTime.unboxToInt(v2)); }
}
