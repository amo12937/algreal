package amo.AlgReal.Field

import amo.AlgReal.{ EuclideanDomainTrait, Unipoly }

trait PrimeFieldModular {
    val p: BigInt
}

class PrimeField[M <: PrimeFieldModular](val n: BigInt)(
    implicit val modular: M,
    bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
) {
    def + (rhs: PrimeField[M]) = PrimeField(n + rhs.n)
    def unary_- = PrimeField(-n)
    def - (rhs: PrimeField[M]) = PrimeField(n - rhs.n)
    def * (rhs: PrimeField[M]) = PrimeField(n * rhs.n)
    def inverse = {
        val (r, s, _) = bigIntEuclideanDomain.exgcd(n, modular.p)
        PrimeField(if (r < 0) -s else s)
    }
    def / (rhs: PrimeField[M]) = this * rhs.inverse
    def pow(m: Int) = PrimeField(n pow m)

    override def toString: String = s"$n (mod ${modular.p})"
    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[PrimeField[M]]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: PrimeField[M] =>
            r.canEqual(this) &&
            n == r.n &&
            modular.p == r.modular.p
        case _ => false
    }

    def toBigInt: BigInt = n
}

trait PrimeFieldFactory[M <: PrimeFieldModular] {
    implicit val bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    implicit val modular: M

    def create(n: BigInt): PrimeField[M] = PrimeField(n)
}

trait PrimeFieldTrait[M <: PrimeFieldModular]
extends FiniteFieldTrait[PrimeField[M]]
with PrimeFieldFactory[M] {
    implicit val bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    implicit val modular: M

    def equiv(a: PrimeField[M], b: PrimeField[M]): Boolean = a == b

    lazy val zero = PrimeField(0)
    lazy val one = PrimeField(1)

    def add(a: PrimeField[M], b: PrimeField[M]) = a + b
    def negate(a: PrimeField[M]) = -a
    def times(a: PrimeField[M], b: PrimeField[M]) = a * b
    def timesN(a: PrimeField[M], n: Int) = a * PrimeField(n)
    def pow(a: PrimeField[M], n: Int) = a pow n

    def divide(a: PrimeField[M], b: PrimeField[M]) = a / b

    def order(t: PrimeField[M]) = characteristic(t)
    def characteristic(t: PrimeField[M]) = modular.p
}

object PrimeField {
    def apply[M <: PrimeFieldModular](n: BigInt)(
        implicit modular: M,
        bigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    ): PrimeField[M] =
        if (n < 0) new PrimeField(modular.p + (n % modular.p))
        else new PrimeField(n % modular.p)

    def makePrimeField[M <: PrimeFieldModular](m: M)(
        implicit implicitlyBigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    ) = new PrimeFieldTrait[M] {
        val bigIntEuclideanDomain = implicitlyBigIntEuclideanDomain
        val modular = m
    }

    trait implicits {
        trait M extends PrimeFieldModular
        implicit val pf: PrimeFieldTrait[M]
        implicit lazy val nToPf = pf.fromInt _
        implicit lazy val pfUnipoly = Unipoly.makeUnipoly[PrimeField[M]]
        implicit lazy val nToPfUnipoly = pfUnipoly.fromInt _
    }

    def makeImplicits(prime: BigInt)(
        implicit implicitlyBigIntEuclideanDomain: EuclideanDomainTrait[BigInt]
    ) = new implicits {
        val pf = makePrimeField(new M { val p = prime })
    }
}
