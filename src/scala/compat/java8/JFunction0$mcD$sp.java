
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction0$mcD$sp extends JFunction0 {
    abstract double apply$mcD$sp();

    default Object apply() { return (Double) apply$mcD$sp(); }
}
