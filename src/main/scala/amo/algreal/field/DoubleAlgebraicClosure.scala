package amo.algreal.field

case class AllowableError(err: Double = 1e-5) {
    def round(d: Double): Long = (d / err).round
    def allow(l: Double, r: Double): Boolean = round(l) == round(r)
    def hash(d: Double): Int = round(d).hashCode
}

class DoubleWrapper(val d: Double)(implicit val err: AllowableError) extends Equals {
    def canEqual(rhs: Any): Boolean = rhs.isInstanceOf[DoubleWrapper]
    override def equals(rhs: Any): Boolean = rhs match {
        case r: DoubleWrapper =>
            r.canEqual(this) && err.allow(this.d, r.d)
        case _ => false
    }
    override def hashCode(): Int = err.hash(d)

    def + (rhs: DoubleWrapper) = new DoubleWrapper(d + rhs.d)
    def unary_- = new DoubleWrapper(-d)
    def - (rhs: DoubleWrapper) = this + (-rhs)
    def * (rhs: DoubleWrapper) = new DoubleWrapper(d * rhs.d)
    def / (rhs: DoubleWrapper) = new DoubleWrapper(d / rhs.d)
    def pow(n: Int) = new DoubleWrapper(scala.math.pow(d, n))
    def nthRoot(n: Int) = new DoubleWrapper(scala.math.pow(d, 1.0 / n))

    override def toString = d.toString
}

class DoubleAlgebraicClosure(implicit val err: AllowableError)
extends AlgebraicClosureTrait[DoubleWrapper]
with Ordering[DoubleWrapper] {
    override def equiv(a: DoubleWrapper, b: DoubleWrapper) = a == b
    def compare(a: DoubleWrapper, b: DoubleWrapper) =
        if (a == b) 0
        else a.d.compare(b.d)

    val zero = new DoubleWrapper(0)
    val one = new DoubleWrapper(1)

    def add(a: DoubleWrapper, b: DoubleWrapper) = a + b
    def negate(a: DoubleWrapper) = -a
    def times(a: DoubleWrapper, b: DoubleWrapper) = a * b
    def timesN(a: DoubleWrapper, n: Int) = new DoubleWrapper(a.d * n)
    def pow(a: DoubleWrapper, n: Int) = a.pow(n)

    def divide(a: DoubleWrapper, b: DoubleWrapper) = a / b
    def nthRoot(a: DoubleWrapper, n: Int) = a.nthRoot(n)
}

object DoubleAlgebraicClosure {
    trait implicits {
        implicit val doubleAllowableError = AllowableError()
        implicit val doubleAlgebraicClosure = new DoubleAlgebraicClosure
    }
    object implicits extends implicits
}
