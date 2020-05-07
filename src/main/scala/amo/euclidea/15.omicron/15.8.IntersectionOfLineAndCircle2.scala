package amo.euclidea.omicron

import amo.algreal.field.ConstructibleTrait
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    CircleCommandProvider,
}
import amo.geometry.figures.{ Point, Circle }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}

object Omicron_15_8_IntersectionOfLineAndCircle2 {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(5, 0)
        val c1: Circle[T] = Circle(p2, 1)

        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2), circles = Set(c1))
        )

        val p3: Point[T] = Point(4, 0)
        val p4: Point[T] = Point(6, 0)
        val answer = new FullFillEnvironmentAnswer[T](
            Vector(Board(Set(p3, p4))),
            Cost(7, 7)
        )

        val commands: Vector[CommandProvider[T]] = Vector(
            CircleCommandProvider()
        )

        new Problem[T] {
            val problemL = ProblemL(initialEnvironment, commands, answer)
            val problemE = ProblemE(initialEnvironment, commands, answer)
        }
    }
}
