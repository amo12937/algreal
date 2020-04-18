package amo.AlgReal

import scala.math.Ordering

object BigInteger {
    object Implicits {
        implicit val bigInt =
            new GcdDomainTrait[BigInt]
            with Ordering[BigInt] {
                def compare(x: BigInt, y: BigInt): Int =
                    if (x < y) -1
                    else if (x > y) 1
                    else 0

                val zero = 0
                val one = 1

                def add(a: BigInt, b: BigInt) = a + b
                def negate(a: BigInt): BigInt = -a
                def times(a: BigInt, b: BigInt) = a * b
                def timesN(a: BigInt, n: Int) = a * n
                def pow(a: BigInt, n: Int) = a pow n

                def divide(a: BigInt, b: BigInt) = a / b

                def gcd(a: BigInt, b: BigInt) = a gcd b
                def content(xs: Seq[BigInt]) = xs.reduceLeft(gcd)
            }
    }
}
