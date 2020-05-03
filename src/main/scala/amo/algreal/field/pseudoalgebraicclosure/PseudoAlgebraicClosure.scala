package amo.algreal.field.pseudoalgebraicclosure

import amo.algreal.field.AlgebraicClosureTrait

sealed trait PseudoAlgebraicClosure[T] {
    implicit val algebraicClosure: AlgebraicClosureTrait[T]

    def calculate: PseudoAlgebraicClosure[T]
    def assign(
        vs: Map[Indeterminate[T], PseudoAlgebraicClosure[T]]
    ): PseudoAlgebraicClosure[T]
    def indeterminates: Set[Indeterminate[T]]

    def + (rhs: PseudoAlgebraicClosure[T]): PseudoAlgebraicClosure[T] = Add(this, rhs)
    def unary_- = this match {
        case Negate(v) => v
        case _ => Negate(this)
    }
    def - (rhs: PseudoAlgebraicClosure[T]): PseudoAlgebraicClosure[T] = this + (-rhs)
}

trait UnaryOp[T] extends PseudoAlgebraicClosure[T] {
    val lhs: PseudoAlgebraicClosure[T]
    val runUnaryFunc: T => T
    val createNew: PseudoAlgebraicClosure[T] => PseudoAlgebraicClosure[T]

    def calculate = lhs.calculate match {
        case Scalar(l) => Scalar(runUnaryFunc(l))
        case l => createNew(l)
    }
    def assign(
        vs: Map[Indeterminate[T], PseudoAlgebraicClosure[T]]
    ) = createNew(lhs.assign(vs))
    def indeterminates = lhs.indeterminates
}

trait BinaryOp[T] extends PseudoAlgebraicClosure[T] {
    val lhs: PseudoAlgebraicClosure[T]
    val rhs: PseudoAlgebraicClosure[T]
    val runBinaryFunc: (T, T) => T
    val createNew: (PseudoAlgebraicClosure[T], PseudoAlgebraicClosure[T]) => PseudoAlgebraicClosure[T]

    def calculate = (lhs.calculate, rhs.calculate) match {
        case (Scalar(l), Scalar(r)) => Scalar(runBinaryFunc(l, r))
        case (l, r) => createNew(l, r)
    }
    def assign(
        vs: Map[Indeterminate[T], PseudoAlgebraicClosure[T]]
    ) = createNew(lhs.assign(vs), rhs.assign(vs))
    def indeterminates = lhs.indeterminates ++ rhs.indeterminates
}

case class Scalar[T](t: T)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends PseudoAlgebraicClosure[T] {
    def calculate = this
    def assign(
        vs: Map[Indeterminate[T], PseudoAlgebraicClosure[T]]
    ) = this
    def indeterminates = Set.empty
}

case class Indeterminate[T](id: String)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends PseudoAlgebraicClosure[T] {
    def calculate = this
    def assign(
        vs: Map[Indeterminate[T], PseudoAlgebraicClosure[T]]
    ): PseudoAlgebraicClosure[T] = vs.getOrElse(this, this)
    def indeterminates = Set(this)
}

case class Add[T](
    lhs: PseudoAlgebraicClosure[T], rhs: PseudoAlgebraicClosure[T]
)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends BinaryOp[T] {
    val runBinaryFunc = algebraicClosure.add
    val createNew = Add[T]
}

case class Negate[T](
    lhs: PseudoAlgebraicClosure[T]
)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends UnaryOp[T] {
    val runUnaryFunc = algebraicClosure.negate
    val createNew = Negate[T]
}

object Negate {
    def apply[T](lhs: PseudoAlgebraicClosure[T])(
        implicit algebraicClosure: AlgebraicClosureTrait[T]
    ): PseudoAlgebraicClosure[T] = lhs match {
        case Negate(l) => l
        case _ => new Negate(lhs)
    }
}

case class Multiply[T](
    lhs: PseudoAlgebraicClosure[T], rhs: PseudoAlgebraicClosure[T]
)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends BinaryOp[T] {
    val runBinaryFunc = algebraicClosure.times
    val createNew = Multiply[T]
}

case class Pow[T](
    lhs: PseudoAlgebraicClosure[T], n: Int
)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends UnaryOp[T] {
    val runUnaryFunc = algebraicClosure.pow(_, n)
    val createNew = Pow[T](_, n)
}

object Pow {
    def apply[T](
        lhs: PseudoAlgebraicClosure[T], n: Int
    )(
        implicit algebraicClosure: AlgebraicClosureTrait[T]
    ): PseudoAlgebraicClosure[T] = lhs match {
        case Pow(l, m) => Pow(l, n * m)
        case Multiply(l, r) => Multiply(Pow(l, n), Pow(r, n))
        case _ => new Pow(lhs, n)
    }
}

case class Inverse[T](
    lhs: PseudoAlgebraicClosure[T]
)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends UnaryOp[T] {
    val runUnaryFunc = algebraicClosure.divide(algebraicClosure.one, _)
    val createNew = Inverse[T]
}

object Inverse {
    def apply[T](
        lhs: PseudoAlgebraicClosure[T]
    )(
        implicit algebraicClosure: AlgebraicClosureTrait[T]
    ): PseudoAlgebraicClosure[T] = lhs match {
        case Inverse(l) => l
        case Multiply(l, r) => Multiply(Inverse(l), Inverse(r))
        case _ => new Inverse(lhs)
    }
}

case class NthRoot[T](
    lhs: PseudoAlgebraicClosure[T], n: Int
)(
    implicit val algebraicClosure: AlgebraicClosureTrait[T]
) extends UnaryOp[T] {
    val runUnaryFunc = algebraicClosure.nthRoot(_, n)
    val createNew = NthRoot[T](_, n)
}

object NthRoot {
    def apply[T](
        lhs: PseudoAlgebraicClosure[T], n: Int
    )(
        implicit algebraicClosure: AlgebraicClosureTrait[T]
    ): PseudoAlgebraicClosure[T] = lhs match {
        case NthRoot(l, m) => NthRoot(l, n * m)
        case _ => new NthRoot(lhs, n)
    }
}
