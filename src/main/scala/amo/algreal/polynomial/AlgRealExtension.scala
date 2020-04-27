package amo.algreal.polynomial

import amo.algreal.{ AlgReal, BigInteger, GcdDomainTrait, Resultant }

object AlgRealExtension {
    trait implicits {
        this: BigInteger.implicits
        with Unipoly.implicits =>
        implicit lazy val mulPolyUniPolyBigInt: GcdDomainTrait[MulPoly[Unipoly[BigInt]]] =
            MulPoly.makeMulPoly[Unipoly[BigInt]]
        lazy val resultantMulPoly = new Resultant[MulPoly[Unipoly[BigInt]]]

        implicit class UnipolyAlgRealExtension(unipoly: Unipoly[AlgReal]) {
            def toMulPoly: MulPoly[Unipoly[BigInt]] =
                Iterator.tabulate(unipoly.cs.length)(i =>
                    mulPolyUniPolyBigInt.times(MulPoly.mulInd(i), MulPoly.Scalar(Unipoly.ind[BigInt] pow i))
                ).foldLeft(mulPolyUniPolyBigInt.zero)(mulPolyUniPolyBigInt.add(_, _))

            def elimN: Unipoly[BigInt] =
                unipoly.cs
                    .map(_.definingPolynomial)
                    .foldLeft(toMulPoly)((m, v) => {
                        resultantMulPoly.resultant(
                            m.toUnipoly,
                            v.mapCoeff(c => MulPoly.Scalar(Unipoly(c)))
                        )
                    }).scalarPart
        }
    }
}
