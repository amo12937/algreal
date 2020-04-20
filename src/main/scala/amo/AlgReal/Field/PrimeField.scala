package amo.AlgReal.Field

import amo.AlgReal.EuclideanDomainTrait

class PrimeField(val n: BigInt)(
    implicit val modular: PrimeFieldModular,
    bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
) {
    def + (rhs: PrimeField) = PrimeField(n + rhs.n)
    def unary_- = PrimeField(-n)
    def - (rhs: PrimeField) = PrimeField(n - rhs.n)
    def * (rhs: PrimeField) = PrimeField(n * rhs.n)
    def inverse = {
        val (r, s, _) = bigIntEuclideanDomain.exgcd(n, modular.p)
        PrimeField(if (r < 0) -s else s)
    }
    def / (rhs: PrimeField) = this * rhs.inverse
    def pow(m: Int) = PrimeField(n pow m)

    override def toString: String = s"$n (mod ${modular.p})"
    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[PrimeField]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: PrimeField =>
            r.canEqual(this) &&
            n == r.n &&
            modular.p == r.modular.p
        case _ => false
    }

    def toBigInt: BigInt = n
}

class PrimeFieldModular(val p: BigInt)

trait PrimeFieldCreator {
    implicit val bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    implicit val modular: PrimeFieldModular

    def create(n: BigInt): PrimeField = PrimeField(n)
}

object PrimeField {
    def apply(n: BigInt)(
        implicit modular: PrimeFieldModular,
        bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    ): PrimeField =
        if (n < 0) new PrimeField(modular.p + (n % modular.p))
        else new PrimeField(n % modular.p)

    def makePrimeField(p: BigInt)(
       implicit implicitlyBigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    ) = new FiniteFieldTrait[PrimeField] with PrimeFieldCreator {
        val bigIntEuclideanDomain = implicitlyBigIntEuclideanDomain
        implicit val modular = new PrimeFieldModular(p)

        def equiv(a: PrimeField, b: PrimeField): Boolean = a == b

        val zero = PrimeField(0)
        val one = PrimeField(1)

        def add(a: PrimeField, b: PrimeField) = a + b
        def negate(a: PrimeField) = -a
        def times(a: PrimeField, b: PrimeField) = a * b
        def timesN(a: PrimeField, n: Int) = a * PrimeField(n)
        def pow(a: PrimeField, n: Int) = a pow n

        def divide(a: PrimeField, b: PrimeField) = a / b

        def order(t: PrimeField) = characteristic(t)
        def characteristic(t: PrimeField) = modular.p
    }
}
