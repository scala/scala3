
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcZI$sp extends JFunction1<Object, Object> {
    abstract boolean apply$mcZI$sp(int v1);

    default Object apply(Object t) { return (Boolean) apply$mcZI$sp(scala.runtime.BoxesRunTime.unboxToInt(t)); }
}
