package amo.algreal

import scala.util.{ Random => ScalaRandom }

import amo.algreal.factors.Hensel
import amo.algreal.Field.{ FieldTrait, QuotientField, QuotientFieldOrderingExtension }
import amo.algreal.polynomial.{ AlgRealExtension, StrumExtension, Unipoly }
import amo.util.Random

sealed trait AlgReal extends Equals with Ordered[AlgReal] {
    def definingPolynomial: Unipoly[BigInt]
    def isolatingInterval: Interval[QuotientField[BigInt]]

    def intervals: Iterator[Interval[QuotientField[BigInt]]]

    def compare(rhs: AlgReal): Int

    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[AlgReal]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: AlgReal => r.canEqual(this) && compare(r) == 0
        case _ => false
    }

    def + (rhs: AlgReal): AlgReal
    def unary_-(): AlgReal
    def - (rhs: AlgReal): AlgReal = this + (-rhs)

    def * (rhs: AlgReal): AlgReal
    def inverse: AlgReal
    def / (rhs: AlgReal): AlgReal = this * rhs.inverse

    def pow(n: BigInt): AlgReal
    def nthRoot(n: Int): AlgReal
    def powRat(q: QuotientField[BigInt]) =
        if (q.denom < Int.MaxValue) nthRoot(q.denom.intValue).pow(q.num)
        else new RuntimeException(s"to big denom: ${q}")
    def sqrt = nthRoot(2)
}

object AlgRealImplicits
extends BigInteger.implicits
with AlgRealExtension.implicits
with Closure.implicits
with QuotientField.implicits
with QuotientFieldOrderingExtension.implicits
with Random.implicits
with StrumExtension.implicits
with Unipoly.implicits {
    val qUnipoly = Unipoly.makeUnipoly[QuotientField[BigInt]]
}

object AlgReal {
    import AlgRealImplicits._

    val r = new ScalaRandom
    val hensel: Hensel = new Hensel(r.nextBigInt(_))
    val resultantPoly = new Resultant[Unipoly[BigInt]]

    implicit lazy val algRealField =
        new FieldTrait[AlgReal]
        with Ordering[AlgReal] {
            def compare(x: AlgReal, y: AlgReal) = x compare y

            val zero = Rat(0)
            val one = Rat(1)

            def add(a: AlgReal, b: AlgReal) = a + b
            def negate(a: AlgReal) = -a
            def times(a: AlgReal, b: AlgReal) = a * b
            def timesN(a: AlgReal, n: Int) = a * Rat(n)
            def pow(a: AlgReal, n: Int) = a.pow(n)

            def divide(a: AlgReal, b: AlgReal) = a / b
        }

    lazy val algRealQuotientField = QuotientField.makeQuotientField[AlgReal]

    case class Rat(val r: QuotientField[BigInt]) extends AlgReal {
        val f = Unipoly(-r.num, r.denom)
        val i = Interval(r - 1, r + 1)

        def definingPolynomial = f
        def isolatingInterval = i
        def intervals = {
            val iv = Interval(r, r)
            Iterator.continually(iv)
        }

        def compare(rhs: AlgReal) = rhs match {
            case Rat(rhsR) => r.compare(rhsR)
            case AlgRealPoly(rhsF, rhsS, rhsI) => {
                if (r <= rhsI.left) -1
                else if (rhsI.right <= r) 1
                else if (rhsF.countRealRootsBetween(r, rhsI.right) == 1) -1
                else if (rhsF.valueAt(r) == 0) 0
                else 1
            }
        }

        def + (rhs: AlgReal) = rhs match {
            case Rat(rhsR) => Rat(r + rhsR)
            case x: AlgRealPoly => x + this
        }

        def unary_- = Rat(-r)

        def * (rhs: AlgReal) = rhs match {
            case Rat(rhsR) => Rat(r * rhsR)
            case x: AlgRealPoly => x * this
        }

        def inverse = Rat(r.inverse)

        def pow(n: BigInt) = if (n < Int.MaxValue) Rat(r.pow(n.intValue)) else {
            val (q, r) = n /% 2
            val x = pow(q)
            if (r == 1) x * x * this else x * x
        }

        def nthRoot(n: Int) = {
            lazy val x = Unipoly.ind[BigInt]
            if (n == 0) throw new ArithmeticException("0th root")
            else if (n < 0) inverse.nthRoot(-n)
            else if (r == 0) Rat(0)
            else if (r > 0) realRoots(
                r.denom * (x^n) - r.num, Closure(0), Closure.PositiveInfinity
            ).toVector match {
                case Vector(res) => res
                case l => throw new RuntimeException(s"none or multiple roots $l")
            }
            else if (n % 2 == 1) realRoots(
                r.denom * (x^n) - r.num, Closure.NegativeInfinity, Closure(0)
            ).toVector match {
                case Vector(res) => res
                case l => throw new RuntimeException(s"none or multiple roots $l")
            }
            else throw new RuntimeException("root negative")
        }
    }

    val orderingQ = implicitly[Ordering[QuotientField[BigInt]]]
    case class AlgRealPoly(
        val f: Unipoly[BigInt],
        val s: Int,
        val i: Interval[QuotientField[BigInt]]
    ) extends AlgReal {
        def definingPolynomial = f
        def isolatingInterval = i

        def intervals = f.intervalsWithSign(s, i)

        def compare(rhs: AlgReal) = rhs match {
            case x: Rat => -x.compare(this)
            case AlgRealPoly(_, _, rhsI) if (i.right <= rhsI.left) => -1
            case AlgRealPoly(_, _, rhsI) if (rhsI.right <= i.left) => 1
            case AlgRealPoly(rhsF, _, rhsI) if (f.gcd(rhsF).countRealRootsBetween(
                orderingQ.max(i.left, rhsI.left),
                orderingQ.min(i.right, rhsI.right)
            ) == 1) => 0
            case _ => this.intervals.zip(rhs.intervals).flatMap({ case (ivL, ivR) =>
                if (ivL.right <= ivR.left) Some(-1)
                else if (ivR.right <= ivL.left) Some(1)
                else None
            }).next
        }

        def + (rhs: AlgReal) = rhs match {
            case x: Rat => mkAlgReal(
                f.homogeneousCompP(x.definingPolynomial, x.r.denom)._1,
                i + x.r
            )
            case AlgRealPoly(rhsF, rhsS, rhsI) => {
                val fy = f.mapCoeff(Unipoly(_)) // f(y)
                val fxy = fy.composition(
                    Unipoly(Unipoly.ind[BigInt], -Unipoly.one[BigInt])
                ) // f(x - y)
                val gy = rhsF.mapCoeff(Unipoly(_)) // g(y)
                val res = resultantPoly.resultant(fxy, gy).squareFree
                val ivs = intervals.zip(rhs.intervals).map {
                    case (l, r) => l + r
                }
                mkAlgRealWithIntervals(res, ivs)
            }
        }

        def unary_- = AlgRealPoly(f.composition(-Unipoly.ind[BigInt]), -s, -i)

        def * (rhs: AlgReal) = rhs match {
            case Rat(r) => if (r == QuotientField.zero[BigInt]) Rat(r) else mkAlgReal(
                Unipoly(f.cs.toIterator
                    .zip(Iterator.iterate(r.num pow f.degreeInt)(_ / r.num * r.denom))
                    .map({case (c, n) => c * n})
                    .toVector
                ), i * r
            )
            case AlgRealPoly(rhsF, rhsS, rhsI) => {
                val fxy = Unipoly(f.cs.zipWithIndex.map({case (c, i) =>
                    Unipoly(c).shift(i)
                }).reverse)  // y^n f(x/y)
                val gy = rhsF.mapCoeff(Unipoly(_)) // g(y)
                val res = resultantPoly.resultant(fxy, gy).squareFree
                val ivs = intervals.zip(rhs.intervals).map {
                    case (l, r) => l * r
                }
                mkAlgRealWithIntervals(res, ivs)
            }
        }

        def inverse = mkAlgReal(Unipoly(f.cs.reverse), i.inverse)

        def pow(n: BigInt) =
            if (n == 0) Rat(1)
            else if (n == 1) this
            else if (n < 0) pow(-n).inverse
            else {
                val fq = f.mapCoeff(QuotientField(_))
                val g = qUnipoly.powMod(Unipoly.ind, n, fq)
                val k = Rat(fq.leadingCoefficient).pow(n - f.degreeInt + 1)
                g.mapCoeff[AlgReal](c => Rat(c) / k).valueAt(this)
            }

        def nthRoot(n: Int) = {
            lazy val x = Unipoly.ind[BigInt]
            if (n == 0) throw new ArithmeticException("0th root")
            else if (n < 0) inverse.nthRoot(-n)
            else if (this == Rat(0)) Rat(0)
            else if (this > Rat(0)) realRoots(
                f.composition(x^n), Closure(0), Closure.PositiveInfinity
            ).filter(res => {
                val u = res pow n
                Rat(i.left) <= u && u <= Rat(i.right)
            }).toVector match {
                case Vector(res) => res
                case l => throw new RuntimeException(s"none or multiple roots $l")
            }
            else if (n % 2 == 1) realRoots(
                f.composition(x^n), Closure.NegativeInfinity, Closure(0)
            ).filter(res => {
                val u = res pow n
                Rat(i.left) <= u && u <= Rat(i.right)
            }).toVector match {
                case Vector(res) => res
                case l => throw new RuntimeException(s"none or multiple roots $l")
            }
            else throw new RuntimeException("root negative")
        }

        override def toString =
            s"AlgRealPoly(${f.toStringWithInd("x")}, (${i.left}, ${i.right}))"
    }

    def mkAlgReal(
        f: Unipoly[BigInt],
        iv: Interval[QuotientField[BigInt]]
    ): AlgReal = {
        if (iv.left == iv.right && f.valueAt(iv.left) == 0) Rat(iv.left)
        else if (iv.contains(0) && f.valueAt(0) == 0) Rat(0)
        else if (f.degreeInt == 1) f.cs match {
            case Vector(a, b) => Rat(QuotientField(-a, b))
        }
        else {
            val s = f.signAt(iv.right) match {
                case 0 => f.diff.signAt(iv.right)
                case k => k
            }
            f.intervalsWithSign(s, iv)
                .find(iv2 => !iv2.contains(0))
                .map(AlgRealPoly(f, s, _))
                .getOrElse(Rat(0))
        }
    }

    def mkAlgRealWithIntervals(
        f2: Unipoly[BigInt],
        ivs: Iterator[Interval[QuotientField[BigInt]]]
    ): AlgReal = {
        val ivBuffered = ivs.buffered
        val ivFirst = ivBuffered.head
        (for {
            iv <- ivBuffered.find(i => f2.countRealRootsBetween(i.left, i.right) == 1)
            f <- hensel.factor(f2).find(_.countRealRootsBetween(iv.left, iv.right) == 1)
        } yield mkAlgReal(f, iv)) match {
            case Some(x) => x
            case None => {
                val fStr = f2.toStringWithInd("x")
                throw new RuntimeException(s"cannot find root of ${fStr} between ${ivFirst}")
            }
        }
    }

    def bisect(
        f: Unipoly[BigInt],
        seq: Vector[Unipoly[BigInt]],
        iv: Interval[QuotientField[BigInt]],
        i: Int,
        j: Int
    ): Iterator[AlgReal] =
        if (i <= j) Iterator.empty
        else if (i == j + 1) Iterator(mkAlgReal(f, iv))
        else {
            val c = iv.middle
            val k = StrumExtension.varianceAt(c, seq)
            bisect(f, seq, Interval(iv.left, c), i, k) ++ bisect(f, seq, Interval(c, iv.right), k, j)
        }

    def realRootsBetween(
        f: Unipoly[BigInt],
        lb: Closure[QuotientField[BigInt]],
        ub: Closure[QuotientField[BigInt]]
    ): Iterator[AlgReal] = if (f.degreeInt <= 0) Iterator.empty else {
        val f2 = f.squareFree
        val seq = f2.negativePRS(f2.diff).toVector
        val b = f2.rootBound
        val boundIv = Interval(-b, b).intersect(Interval(lb, ub))

        bisect(
            f, seq, boundIv,
            StrumExtension.varianceAt(boundIv.left, seq),
            StrumExtension.varianceAt(boundIv.right, seq)
        )
    }

    def realRoots(
        f: Unipoly[BigInt],
        lb: Closure[QuotientField[BigInt]] = Closure.NegativeInfinity,
        ub: Closure[QuotientField[BigInt]] = Closure.PositiveInfinity
    ): Iterator[AlgReal] =
        if (f.degreeInt <= 0) Iterator.empty else for {
            f2 <- Iterator(f.squareFree)
            g <- hensel.factor(f2)
            x <- realRootsBetween(
                g, lb, ub
            )
        } yield x

    def realRoots(
        f: Unipoly[AlgReal]
    ): Iterator[AlgReal] =
        if (f.degreeInt <= 0) Iterator.empty else (for {
            f2 <- Iterator(f.squareFree)
            x <- realRoots(f2.elimN)
        } yield x).filter(x => {
            val iv = x.isolatingInterval
            f.countRealRootsBetween(
                Rat(iv.left),
                Rat(iv.right)
            ) == 1
        })

    trait implicits {
        implicit val algReal = algRealField
        implicit val nToAlgReal = algReal.fromInt _
    }
}
