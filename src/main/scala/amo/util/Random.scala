package amo.util

import scala.annotation.tailrec
import scala.util.{ Random => ScalaRandom }

object Random {
    trait implicits {
        implicit class RandomOp(val r: ScalaRandom) {
            def nextBigInt(n: BigInt): BigInt =
                if (n <= 1) 0
                else if (n <= Int.MaxValue) r.nextInt(n.toInt)
                else {
                    val l = n.bitLength + 1
                    @tailrec
                    def tailRec(): BigInt = {
                        val x = BigInt(l, r)
                        if (x < n) x else tailRec()
                    }
                    tailRec()
                }
        }
    }
}
