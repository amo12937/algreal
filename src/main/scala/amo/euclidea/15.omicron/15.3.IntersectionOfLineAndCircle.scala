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

object Omicron_15_3_IntersectionOfLineAndCircle {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(-10, 3)
        val p3: Point[T] = Point(11, 3)
        val c1: Circle[T] = Circle(p1, 5)

        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2, p3), circles = Set(c1))
        )

        val p4: Point[T] = Point(-4, 3)
        val p5: Point[T] = Point(4, 3)
        val answer = new FullFillEnvironmentAnswer[T](
            Vector(Board(Set(p4, p5))),
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
