package amo.AlgReal

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import amo.implicits._

class PrimeSpec extends AnyWordSpec with Matchers {
    "apply" should {
        "return Some[Prime] if prime number supplied" in {
            Prime(2).map(_.n) should be(Some(2))
            Prime(3).map(_.n) should be(Some(3))
            Prime(101).map(_.n) should be(Some(101))
            val mersenne = BigInt(2).pow(127) - 1
            Prime(mersenne).map(_.n) should be(Some(mersenne))
        }

        "return None if not prime number supplied" in {
            Prime(-1) should be(None)
            Prime(0) should be(None)
            Prime(1) should be(None)
            Prime(4) should be(None)
            Prime(100) should be(None)
        }
    }

    "primes" should {
        "return iterator of prime number" in {
            Prime.primes.drop(99).next.n should be(541)
            Prime.primes.drop(9999).next.n should be(104729)
        }
    }
}
