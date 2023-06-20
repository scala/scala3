package tests.functionTypeSignatures

type A = ((Int, Int)) => Int

type B = (Int | String) => Int

type B1 = Int | String => Int //expected: type B1 = (Int | String) => Int

type C = (Int & String) => Int

type C1 = Int & String => Int //expected: type C1 = (Int & String) => Int

type D = Int | (String => Int)

type E = (A => B) => B

