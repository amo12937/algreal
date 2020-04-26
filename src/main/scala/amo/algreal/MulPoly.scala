package amo.algreal

sealed trait MulPoly[T] {
    def toUnipoly: Unipoly[MulPoly[T]]

    def map[S](func: T => S)(
        implicit gcdDomainMS: GcdDomainTrait[MulPoly[S]]
    ): MulPoly[S]

    def flatMap[S](func: T => MulPoly[S])(
        implicit gcdDomainMS: GcdDomainTrait[MulPoly[S]]
    ): MulPoly[S]
}

object MulPoly {
    case class Poly[T](f: Unipoly[MulPoly[T]])(
        implicit gcdDomainM: GcdDomainTrait[MulPoly[T]]
    ) extends MulPoly[T] {
        def toUnipoly = f

        def map[S](func: T => S)(
            implicit gcdDomainMS: GcdDomainTrait[MulPoly[S]]
        ): MulPoly[S] = Poly(f.mapCoeff(_.map(func)))

        def flatMap[S](func: T => MulPoly[S])(
            implicit gcdDomainMS: GcdDomainTrait[MulPoly[S]]
        ): MulPoly[S] = Poly(f.mapCoeff(_.flatMap(func)))
    }

    case class Scalar[T](t: T)(
        implicit gcdDomainM: GcdDomainTrait[MulPoly[T]]
    ) extends MulPoly[T] {
        def toUnipoly = Unipoly(this)

        def map[S](func: T => S)(
            implicit gcdDomainMS: GcdDomainTrait[MulPoly[S]]
        ): MulPoly[S] = Scalar(func(t))

        def flatMap[S](func: T => MulPoly[S])(
            implicit gcdDomainMS: GcdDomainTrait[MulPoly[S]]
        ): MulPoly[S] = func(t)
    }

    def apply[T](t: T)(
        implicit gcdDomainM: GcdDomainTrait[MulPoly[T]]
    ): MulPoly[T] = Scalar(t)
    def apply[T](f: Unipoly[T])(
        implicit gcdDomainM: GcdDomainTrait[MulPoly[T]]
    ): MulPoly[T] =
        if (f.cs.length <= 1) MulPoly(f.leadingCoefficient)
        else Poly(f.mapCoeff(MulPoly(_)))
    def apply[T](m: MulPoly[T])(
        implicit gcdDomainM: GcdDomainTrait[MulPoly[T]]
    ): MulPoly[T] = m match {
        case _: Scalar[T] => m
        case Poly(f) => if (f.cs.length <= 1) f.leadingCoefficient else Poly(f)
    }

    def makeMulPoly[T](implicit gcdDomainT: GcdDomainTrait[T]) = new GcdDomainTrait[MulPoly[T]] {
        implicit val gcdDomainM: GcdDomainTrait[MulPoly[T]] = this

        def equiv(a: MulPoly[T], b: MulPoly[T]) = (a, b) match {
            case (Scalar(x), Scalar(y)) => gcdDomainT.equiv(x, y)
            case _ => a.toUnipoly == b.toUnipoly
        }

        val zero = Scalar(gcdDomainT.zero)
        val one = Scalar(gcdDomainT.one)

        def add(a: MulPoly[T], b: MulPoly[T]): MulPoly[T] = (a, b) match {
            case (Scalar(x), Scalar(y)) => Scalar(gcdDomainT.add(x, y))
            case _ => Poly(a.toUnipoly + b.toUnipoly)
        }

        def negate(a: MulPoly[T]): MulPoly[T] = a match {
            case Scalar(x) => Scalar(gcdDomainT.negate(x))
            case Poly(f) => Poly(-f)
        }

        def times(a: MulPoly[T], b: MulPoly[T]) = (a, b) match {
            case (Scalar(x), Scalar(y)) => Scalar(gcdDomainT.times(x, y))
            case _ => Poly(a.toUnipoly * b.toUnipoly)
        }

        def timesN(a: MulPoly[T], n: Int) = a match {
            case Scalar(x) => Scalar(gcdDomainT.timesN(x, n))
            case Poly(f) => Poly(f.scale(Scalar(gcdDomainT.fromInt(n))))
        }

        def pow(a: MulPoly[T], n: Int) = a match {
            case Scalar(x) => Scalar(gcdDomainT.pow(x, n))
            case Poly(f) => Poly(f.pow(n))
        }

        def divide(a: MulPoly[T], b: MulPoly[T]) = (a, b) match {
            case (Scalar(x), Scalar(y)) => Scalar(gcdDomainT.divide(x, y))
            case _ => Poly(a.toUnipoly divide b.toUnipoly)
        }

        def unit(a: MulPoly[T]): MulPoly[T] = a match {
            case Scalar(x) => Scalar(gcdDomainT.negate(x))
            case Poly(f) => unit(f.leadingCoefficient)
        }

        def gcd(a: MulPoly[T], b: MulPoly[T]) = (a, b) match {
            case (Scalar(x), Scalar(y)) => Scalar(gcdDomainT.gcd(x, y))
            case _ => Poly(a.toUnipoly gcd b.toUnipoly)
        }
    }

    def mulInd[T](n: Int)(implicit gcdDomainM: GcdDomainTrait[MulPoly[T]]): MulPoly[T] =
        if (n <= 0) Poly(Unipoly.ind[MulPoly[T]])
        else Poly(Unipoly(mulInd(n - 1)))
}
