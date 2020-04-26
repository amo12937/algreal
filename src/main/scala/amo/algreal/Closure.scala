package amo.algreal

sealed abstract class Closure[+T]

object Closure {
    final case class ClosureValue[+T](t: T) extends Closure[T]
    final case object PositiveInfinity extends Closure[Nothing]
    final case object NegativeInfinity extends Closure[Nothing]

    def apply[T](t: T): Closure[T] = ClosureValue(t)

    trait implicits {
        import scala.language.implicitConversions

        def closureOrdering[T](implicit ordering: Ordering[T]) = new Ordering[Closure[T]] {
            def compare(x: Closure[T], y: Closure[T]): Int = (x, y) match {
                case (NegativeInfinity, NegativeInfinity) => 0
                case (_, NegativeInfinity) => 1
                case (NegativeInfinity, _) => -1
                case (PositiveInfinity, PositiveInfinity) => 0
                case (_, PositiveInfinity) => -1
                case (PositiveInfinity, _) => 1
                case (ClosureValue(xx), ClosureValue(yy)) => ordering.compare(xx, yy)
            }
        }

        implicit val closureIntOrdering = closureOrdering[Int]

        implicit def tToClosure[T](t: T): Closure[T] = ClosureValue(t)
        implicit def closureValueToT[T](c: ClosureValue[T]): T = c.t

        implicit class ClosureOrderingExtension[T](c: Closure[T])(
            implicit orderingT: Ordering[T]
        ) extends Ordered[Closure[T]] {
            val orderingC = closureOrdering[T]
            def compare(rhs: Closure[T]): Int = orderingC.compare(c, rhs)
        }
    }

    object implicits extends implicits
}
