
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package dotty.runtime.function;

@FunctionalInterface
public interface JFunction1$mcVF$sp extends JFunction1<Object, Object> {
    abstract void apply$mcVF$sp(float v1);

    default Object apply(Object t) { apply$mcVF$sp(scala.runtime.BoxesRunTime.unboxToFloat(t)); return scala.runtime.BoxedUnit.UNIT; }
}
