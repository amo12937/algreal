package amo.AlgReal

import java.lang.ArithmeticException

import amo.AlgReal.Field.QuotientField

class Unipoly[T](val cs: Vector[T])(
    implicit gcdDomainT: GcdDomainTrait[T]
) extends Equals {
    implicit val nToRingT = gcdDomainT.fromInt _
    def isZero: Boolean = cs.length <= 0

    override def toString: String =
        if (isZero) s"[${nToRingT(0).toString}]"
        else s"[${cs.mkString(", ")}]"

    def toStringWithInd(s: String)(
        implicit ordering: Ordering[T]
    ): String = cs.zipWithIndex
        .filterNot({ case (c, _) => gcdDomainT.equiv(c, gcdDomainT.zero) })
        .reverse match {
            case Vector() => s"${gcdDomainT.zero}"
            case (cHead, idxHead) +: tail => (
                (
                    (if (ordering.lt(cHead, gcdDomainT.zero)) "-" else "") +
                    absToString(s, cHead, idxHead)
                ) +: tail.map({ case (c, idx) =>
                    " " + signToString(c) + " " + absToString(s, c, idx)
                })
            ).mkString
        }

    def signToString(c: T)(implicit ordering: Ordering[T]) =
        if (ordering.lt(c, gcdDomainT.zero)) "-"
        else "+"

    def absToString(s: String, c: T, idx: Int)(
        implicit ordering: Ordering[T]
    ): String =
        if (ordering.lt(c, gcdDomainT.zero)) absToString(s, gcdDomainT.negate(c), idx)
        else (c, idx) match {
            case (c, 0) => s"${c}"
            case (gcdDomainT.one, 1) => s"${s}"
            case (c, 1) => s"${c}${s}"
            case (gcdDomainT.one, idx) => s"${s}^${idx}"
            case (c, idx) => s"${c}${s}^${idx}"
        }

    lazy val degree: Closure[Int] =
        if (isZero) Closure.NegativeInfinity
        else Closure(degreeInt)

    lazy val degreeInt: Int = cs.length - 1

    lazy val leadingCoefficient: T = cs.lastOption getOrElse 0

    def + (rhs: Unipoly[T]): Unipoly[T] = Unipoly(
        cs.zipAll(rhs.cs, nToRingT(0), nToRingT(0)).map {
            case (l, r) => gcdDomainT.add(l, r)
        }
    )

    def addT(t: T): Unipoly[T] = cs match {
        case Vector() => Unipoly(t)
        case head +: tail => Unipoly(gcdDomainT.add(head, t) +: tail)
    }

    def unary_- = Unipoly(cs.map(gcdDomainT.negate))

    def - (rhs: Unipoly[T]): Unipoly[T] = this + (-rhs)

    def * (rhs: Unipoly[T]): Unipoly[T] =
        if (isZero || rhs.isZero) Unipoly()
        else rhs.cs.foldRight(Unipoly())((c, res) => res.shift() + scale(c))

    def shift(n: Int = 1): Unipoly[T] = Unipoly(
        Vector.fill(n)(nToRingT(0)) ++ cs
    )

    def scale(c: T): Unipoly[T] =
        if (gcdDomainT.equiv(c, 0)) Unipoly()
        else Unipoly(cs.map(gcdDomainT.times(_, c)))

    def unscale(c: T): Unipoly[T] = Unipoly(cs.map(gcdDomainT.divide(_, c)))

    def toMonic(): Unipoly[T] = unscale(leadingCoefficient)

    def isMonic(): Boolean = gcdDomainT.equiv(leadingCoefficient, 1)

    def pow(n: Int): Unipoly[T] =
        if (n < 0) throw new ArithmeticException("Negative exponent")
        else if (n == 0) Unipoly(1)
        else if (n % 2 == 0) (this * this).pow(n / 2)
        else this.pow(n - 1) * this

    def ^ (n: Int): Unipoly[T] = pow(n)

    def valueAt(t: T): T = cs.foldRight(nToRingT(0)){ (c, res) =>
        gcdDomainT.add(c, gcdDomainT.times(res, t))
    }

    def valueAt(q: QuotientField[T])(
        implicit tToF: T => QuotientField[T],
        gcdDomainF: GcdDomainTrait[QuotientField[T]],
    ): QuotientField[T] = mapCoeff(tToF).valueAt(q)

    def composition(g: Unipoly[T]): Unipoly[T] =
        if (g.isZero) Unipoly()
        else cs.foldRight(Unipoly()) { (c, res) =>
            res * g addT c
        }

    def content: T = gcdDomainT.content(cs)

    def contentAndPrimitivePart: (T, Unipoly[T]) = {
        val c = content
        if (gcdDomainT.equiv(c, 1)) (c, this)
        else (c, unscale(c))
    }

    def primitivePart: Unipoly[T] = contentAndPrimitivePart._2

    def monicDivMod(g: Unipoly[T]): (Unipoly[T], Unipoly[T]) = g.degree match {
        case Closure.ClosureValue(d) if (g.isMonic) => {
            import amo.AlgReal.Closure.implicits._
            def tailRec(q: Unipoly[T], r: Unipoly[T]): (Unipoly[T], Unipoly[T]) = {
                if (r.degree < g.degree) (q, r)
                else {
                    val s = Unipoly(r.cs.drop(d))
                    tailRec(q + s, r - g * s)
                }
            }
            tailRec(Unipoly(), this)
        }
        case _ => throw new IllegalArgumentException("g must be monic")
    }

    def pseudoDivMod(g: Unipoly[T]): (Unipoly[T], Unipoly[T]) = g.degree match {
        case Closure.ClosureValue(gd) => {
            import amo.AlgReal.Closure.implicits._
            if (degree < g.degree) (Unipoly(), this)
            else {
                val l = cs.length - gd
                val b = g.leadingCoefficient
                def tailRec(
                    q: Unipoly[T], r: Unipoly[T], i: Int
                ): (Unipoly[T], Unipoly[T]) = if (r.degree < g.degree) {
                    val s = gcdDomainT.pow(b, l - i)
                    (q.scale(s), r.scale(s))
                } else {
                    val p = Unipoly(r.cs.drop(gd))
                    tailRec(q.scale(b) + p, r.scale(b) - p * g, i + 1)
                }
                tailRec(Unipoly(), this, 0)
            }
        }
        case _ => throw new IllegalArgumentException("divide by zero")
    }

    def pseudoDiv(g: Unipoly[T]): Unipoly[T] = pseudoDivMod(g)._1
    def pseudoMod(g: Unipoly[T]): Unipoly[T] = pseudoDivMod(g)._2

    /*
     * beta[i]P[i] = prem(P[i-2], P[i-1])     i = 3, ... k
     * beta[3] = (-1)^(d[1] + 1) = sign(d[1] + 1)
     * beta[i] = -c[i-2] psi[i]^(d[i-2])
     * c[i] = lc(P[i])
     * d[i] = deg(P[i]) - deg(P[i+1])
     * psi[3] = -1
     * psi[i] = -c[i-2]^(d[i-3]) psi[i-1]^(1 - d[i-3])
     *
     * C[i] = lc(P[i-2])
     * psi[i] = -C[i]^(D[i-1]) psi[i-1]^(1 - D[i-1])
     * D[i] = deg(P[i-2]) - deg(P[i-1])
     * beta[i] = -C[i] psi[i]^(D[i])
     */
    def subresultantPRS(rhs: Unipoly[T]): Iterator[(T, Unipoly[T])] = {
        def tailRec(psi: T, d: Int, f: Unipoly[T], g: Unipoly[T]): Iterator[(T, Unipoly[T])] = {
            val rem = f.pseudoMod(g)
            if (rem.isZero) Iterator.empty
            else {
                val c = f.leadingCoefficient
                val psi2 = if (d > 0) gcdDomainT.divide(
                    gcdDomainT.pow(gcdDomainT.negate(c), d),
                    gcdDomainT.pow(psi, d - 1)
                ) else gcdDomainT.divide(
                    gcdDomainT.pow(psi, 1 - d),
                    gcdDomainT.pow(gcdDomainT.negate(c), -d),
                )
                val d2 = f.degreeInt - g.degreeInt
                val beta = gcdDomainT.times(
                    gcdDomainT.negate(c),
                    gcdDomainT.pow(psi2, d2)
                )
                val f2 = g
                val g2 = rem.unscale(beta)
                Iterator((beta, g2)) ++ tailRec(psi2, d2, f2, g2)
            }
        }

        if (rhs.isZero) Iterator.empty
        else {
            val d = degreeInt - rhs.degreeInt
            val beta = nToRingT(2 * (d % 2) - 1)
            val g = pseudoMod(rhs).scale(beta)
            if (g.isZero) Iterator.empty
            else Iterator((beta, g)) ++ tailRec(-1, d, rhs, g)
        }
    }

    def gcdSubresultantPRS(g: Unipoly[T]): Unipoly[T] =
        if (g.isZero) this
        else subresultantPRS(g).toVector.lastOption match {
            case None => g
            case Some((_, p)) => p
        }

    def gcd(g: Unipoly[T]): Unipoly[T] =
        if (degreeInt < g.degreeInt) g.gcd(this) else {
            val (fc, fp) = contentAndPrimitivePart
            val (gc, gp) = g.contentAndPrimitivePart
            fp.gcdSubresultantPRS(gp).primitivePart.scale(gcdDomainT.gcd(fc, gc))
        }

    def divide(g: Unipoly[T]): Unipoly[T] = {
        if (g.isZero) throw new ArithmeticException("divide by zero")
        else {
            val b = g.leadingCoefficient
            def tailRec(q: Unipoly[T], r: Unipoly[T]): Unipoly[T] = {
                if (r.degreeInt < g.degreeInt) q
                else {
                    val p = Unipoly(r.cs.drop(g.degreeInt)).unscale(b)
                    tailRec(q + p, r - g * p)
                }
            }
            tailRec(Unipoly(), this)
        }
    }

    def diff: Unipoly[T] = if (isZero) this else Unipoly(
        cs.zipWithIndex.map {
            case (c, i) => gcdDomainT.timesN(c, i)
        }.drop(1)
    )

    def squareFree: Unipoly[T] = divide(gcd(diff))

    def mapCoeff[S](f: T => S)(
        implicit gcdDomainS: GcdDomainTrait[S]
    ): Unipoly[S] = Unipoly(cs.map(f))

    def canEqual(other: Any): Boolean = other.isInstanceOf[Unipoly[T]]
    override def equals(other: Any): Boolean = other match {
        case rhs: Unipoly[T] =>
            rhs.canEqual(this) &&
            cs.length == rhs.cs.length &&
            cs.zip(rhs.cs).forall {
                case (l, r) => gcdDomainT.equiv(l, r)
            }
        case _ => false
    }
}

object Unipoly {
    def normalize[T](cs: Vector[T])(
        implicit ring: RingTrait[T]
    ): Vector[T] = cs match {
        case heads :+ c if (ring.equiv(c, ring.zero)) => normalize(heads)
        case _ => cs
    }

    def apply[T](cs: Vector[T])(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Unipoly[T] = new Unipoly(normalize(cs))

    def apply[T](cs: T*)(
        implicit gcdDomainT: GcdDomainTrait[T]
    ): Unipoly[T] = Unipoly(cs.toVector)

    def zero[T](implicit gcdDomainT: GcdDomainTrait[T]) = Unipoly() // 0
    def one[T](implicit gcdDomainT: GcdDomainTrait[T]) = Unipoly(gcdDomainT.one) // 1
    def ind[T](implicit gcdDomainT: GcdDomainTrait[T]) = Unipoly(
        gcdDomainT.zero, gcdDomainT.one
    ) // x

    def product[T](fs: Traversable[Unipoly[T]])(
        implicit gcdDomainT: GcdDomainTrait[T]
    ) = fs.foldLeft(one)(_ * _)

    def makeUnipoly[T](
        implicit gcdDomainT: GcdDomainTrait[T]
    ) = new EuclideanDomainTrait[Unipoly[T]] {
        implicit val nToRing = gcdDomainT.fromInt _

        def equiv(a: Unipoly[T], b: Unipoly[T]) = a == b

        val zero = Unipoly.zero
        val one = Unipoly.one

        def add(a: Unipoly[T], b: Unipoly[T]) = a + b
        def negate(a: Unipoly[T]) = -a
        def times(a: Unipoly[T], b: Unipoly[T]) = a * b
        def timesN(a: Unipoly[T], n: Int) = a scale n
        def pow(a: Unipoly[T], n: Int) = a pow n

        def divide(a: Unipoly[T], b: Unipoly[T]) = a divide b
        def unit(a: Unipoly[T]) = Unipoly(gcdDomainT.unit(a.leadingCoefficient))

        def gcd(a: Unipoly[T], b: Unipoly[T]) = a gcd b

        def divMod(a: Unipoly[T], b: Unipoly[T]) = {
            val lc = b.leadingCoefficient
            val f = a.unscale(lc)         // a = lc * f
            val g = b.unscale(lc)         // b = lc * g
            val (q, r) = f.monicDivMod(g) // f = qg + r
            (q, r.scale(lc))              // a = qb + lc*r
            // TODO: test
        }
    }

    trait implicits {
        import scala.language.implicitConversions

        implicit def tToUnipoly[T](t: T)(
            implicit gcdDomainT: GcdDomainTrait[T]
        ): Unipoly[T] = Unipoly(t)
    }
}
