
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcZF$sp extends JFunction1<Object, Object> {
    abstract boolean apply$mcZF$sp(float v1);

    default Object apply(Object t) { return (Boolean) apply$mcZF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); }
}
