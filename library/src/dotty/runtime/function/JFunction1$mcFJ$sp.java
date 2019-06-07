
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcFJ$sp extends JFunction1<Object, Object> {
    abstract float apply$mcFJ$sp(long v1);

    default Object apply(Object t) { return (Float) apply$mcFJ$sp(scala.runtime.BoxesRunTime.unboxToLong(t)); }
}
