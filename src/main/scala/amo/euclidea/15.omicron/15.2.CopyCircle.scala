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

object Omicron_15_2_CopyCircle {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(5, 1)
        val c1: Circle[T] = Circle(p1, 1)

        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2), circles = Set(c1))
        )

        val c2: Circle[T] = Circle(p2, 1)
        val answer = new FullFillEnvironmentAnswer[T](
            Vector(Board(circles = Set(c2))),
            Cost(4, 4)
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
