package amo.AlgReal

import scala.annotation.tailrec

import amo.implicits.{ bigInt }

class Prime(val n: BigInt) extends AnyVal

object Prime {
    def apply(n: BigInt): Option[Prime] =
        if (isPrime(n)) Some(new Prime(n))
        else None

    def primes = (
        primesTo10000 ++ Iterator.iterate(BigInt(10001))(_ + 2).filter(isPrime)
    ).map(new Prime(_))

    def primesTo100 = Iterator(
         2,  3,  5,  7, 11, 13, 17, 19, 23, 29,
        31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
        73, 79, 83, 89, 97
    ).map(BigInt(_))

    def trialDivision(
        ns: Iterator[BigInt],
        n: BigInt
    ): Boolean = {
        @tailrec
        def tailRec(ps: Iterator[BigInt]): Boolean = if (!ps.hasNext) true else {
            val p = ps.next
            val (q, r) = n /% p
            if (r == 0) false
            else if (q < p) true
            else tailRec(ps)
        }
        tailRec(ns)
    }

    def primesTo10000 =
        primesTo100 ++
        Iterator.range(101, 9999, 2)
            .map(BigInt(_))
            .filter(n => trialDivision(primesTo100, n))

    def isTrialDivisionPrime(n: BigInt): Boolean =
        if (n == 2) true
        else trialDivision(
            primesTo10000 ++ Iterator.iterate(BigInt(10001))(_ + 2),
            n
        )

    def isStrongPseudoPrime(n: BigInt, s: BigInt, d: BigInt, base: BigInt): Boolean = {
        val b = bigInt.powMod(base, d, n)
        if (b == 1) true else {
            @tailrec
            def tailRec(ss: BigInt, x: BigInt): Boolean =
                if (x == n - 1) true
                else if (x == 1) false
                else tailRec(ss - 1, (x * x) mod n)
            tailRec(s, b)
        }
    }

    def splitWith(n: BigInt, m: BigInt): (BigInt, BigInt) = {
        @tailrec
        def tailRec(s: BigInt, t: BigInt): (BigInt, BigInt) =
            if (t % m != 0) (s, t)
            else tailRec(s + 1, t / m)
        tailRec(0, n)
    }

    def isMillerRabinPrime(n: BigInt): Boolean = {
        val (s, d) = splitWith(n - 1, 2)
        primesTo100.forall(isStrongPseudoPrime(n, s, d, _))
    }

    val threshold = BigInt("500000000")
    def isPrime(n: BigInt): Boolean =
        if (n < 2) false
        else if (n < threshold) isTrialDivisionPrime(n)
        else isMillerRabinPrime(n)
}
