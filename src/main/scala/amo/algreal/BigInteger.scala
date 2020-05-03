package amo.algreal

import scala.annotation.tailrec
import scala.math.Ordering

import amo.algreal.field.{ QuotientField, QuotientFieldTrait, QuotientFieldOrderingExtension }
import amo.algreal.polynomial.Unipoly

object BigInteger {
    def squareRoot(n: BigInt): BigInt =
        if (n < 0) throw new ArithmeticException("sauare root of negative number")
        else if (n <= 1) n
        else {
            @tailrec
            def tailRec(a: BigInt, b: BigInt): BigInt = {
                val mid = (a + b) >> 1
                mid.pow(2).compare(n) match {
                    case 0 => mid
                    case -1 => {
                        val nextA = mid + 1
                        if (nextA >= b) mid
                        else tailRec(nextA, b)
                    }
                    case 1 => {
                        val nextB = mid - 1
                        if (a >= nextB) nextB
                        else tailRec(a, nextB)
                    }
                }
            }
            tailRec(1, (n >> 5) + 8)
        }

    trait implicits {
        this: Closure.implicits
        with QuotientField.implicits
        with QuotientFieldOrderingExtension.implicits
        with Unipoly.implicits =>

        implicit val bigInt =
            new EuclideanDomainTrait[BigInt]
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
                def unit(a: BigInt) = if (a == 0) 1 else a.signum

                def gcd(a: BigInt, b: BigInt) = a gcd b

                def divMod(a: BigInt, b: BigInt) = a /% b
                override def mod(a: BigInt, b: BigInt) = a % b
            }

        val rational = implicitly[QuotientFieldTrait[BigInt]]
        implicit val nToRationalFieldBigInt = rational.fromInt _
        implicit val nToU = implicitly[RingTrait[Unipoly[BigInt]]].fromInt _
    }
}
