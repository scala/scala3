
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction0$mcF$sp extends JFunction0 {
    abstract float apply$mcF$sp();

    default Object apply() { return (Float) apply$mcF$sp(); }
}
