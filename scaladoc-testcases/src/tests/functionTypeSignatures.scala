package tests.functionTypeSignatures

type A = ((Int, Int)) => Int

type B = (Int | String) => Int

type C = (Int & String) => Int

type E = (A => B) => B

