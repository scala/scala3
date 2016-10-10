
/*
 * Copyright (C) 2012-2014 Typesafe Inc. <http://www.typesafe.com>
 */

package scala.compat.java8;

@FunctionalInterface
public interface JFunction0<R> extends scala.Function0<R> {
    default void $init$() {
    };
    default void apply$mcV$sp() {
        apply();
    }
    default byte apply$mcB$sp() {
        return (Byte) apply();
    }
    default short apply$mcS$sp() {
        return (Short) apply();
    }
    default int apply$mcI$sp() {
        return (Integer) apply();
    }
    default long apply$mcJ$sp() {
        return (Long) apply();
    }
    default char apply$mcC$sp() {
        return (Character) apply();
    }
    default float apply$mcF$sp() {
        return (Float) apply();
    }
    default double apply$mcD$sp() {
        return (Double) apply();
    }
    default boolean apply$mcZ$sp() {
        return (Boolean) apply();
    }
}
