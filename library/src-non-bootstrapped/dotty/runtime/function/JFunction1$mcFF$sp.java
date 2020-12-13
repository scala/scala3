
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcFF$sp extends JFunction1<Object, Object> {
    abstract float apply$mcFF$sp(float v1);

    default Object apply(Object t) { return (Float) apply$mcFF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); }
}
