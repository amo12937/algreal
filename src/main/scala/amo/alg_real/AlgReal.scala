package amo.AlgReal

import scala.util.Random

import amo.implicits._
import amo.AlgReal.Field.{ FieldTrait, QuotientField }
import amo.AlgReal.factors.Hensel

sealed trait AlgReal extends Equals {
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

    def pow(n: Int): AlgReal
}

object AlgReal {
    val r = new Random
    val hensel: Hensel = new Hensel(r.nextBigInt(_))
    val resultantPoly = new Resultant[Unipoly[BigInt]]

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
            case Rat(rhsR) => rational.compare(r, rhsR)
            case AlgRealPoly(rhsF, rhsS, rhsI) => {
                if (rational.lteq(r, rhsI.left)) -1
                else if (rational.lteq(rhsI.right, r)) 1
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

        def pow(n: Int) = Rat(r.pow(n))
    }

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
            case AlgRealPoly(_, _, rhsI) if rational.lteq(i.right, rhsI.left) => -1
            case AlgRealPoly(_, _, rhsI) if rational.lteq(rhsI.right, i.left) => 1
            case AlgRealPoly(rhsF, _, rhsI) if (f.gcd(rhsF).countRealRootsBetween(
                rational.max(i.left, rhsI.left),
                rational.min(i.right, rhsI.right)
            ) == 1) => 0
            case _ => this.intervals.zip(rhs.intervals).flatMap({ case (ivL, ivR) =>
                if (rational.lteq(ivL.right, ivR.left)) Some(-1)
                else if (rational.lteq(ivR.right, ivL.left)) Some(1)
                else None
            }).next
        }

        def + (rhs: AlgReal) = rhs match {
            case x: Rat => mkAlgReal(
                f.composition(x.definingPolynomial),
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

        def unary_- = AlgRealPoly(f.composition(Unipoly.ind), -s, -i)

        def * (rhs: AlgReal) = rhs match {
            case Rat(r) => if (r == 0) Rat(r) else mkAlgReal(
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

        def pow(n: Int) = {
            val g = Unipoly.ind[BigInt].pow(n)
            val k = f.leadingCoefficient.pow(g.degreeInt - f.degreeInt + 1)
            g.pseudoMod(f).mapCoeff[AlgReal](c => Rat(rational.create(c, k))).valueAt(this)
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
            case Vector(a, b) => Rat(rational.create(-a, b))
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
        f: Unipoly[BigInt]
    ): Iterator[AlgReal] =
        if (f.degreeInt <= 0) Iterator.empty else for {
            f2 <- Iterator(f.squareFree)
            g <- hensel.factor(f2)
            x <- realRootsBetween(
                g, Closure.NegativeInfinity, Closure.PositiveInfinity
            )
        } yield x

    trait implicits {
        implicit val algReal =
            new FieldTrait[AlgReal]
            with Ordering[AlgReal] {
                def compare(x: AlgReal, y: AlgReal) = x compare y

                def zero = Rat(0)
                def one = Rat(1)

                def add(a: AlgReal, b: AlgReal) = a + b
                def negate(a: AlgReal) = -a
                def times(a: AlgReal, b: AlgReal) = a * b
                def timesN(a: AlgReal, n: Int) = a * Rat(n)
                def pow(a: AlgReal, n: Int) = a.pow(n)

                def divide(a: AlgReal, b: AlgReal) = a / b
            }
    }
    object implicits extends implicits
}
