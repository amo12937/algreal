package amo.euclidea.alpha

import amo.algreal.field.ConstructibleTrait
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    CircleCommandProvider,
    LineCommandProvider,
    VerticalBisectorCommandProvider
}
import amo.geometry.figures.{ Circle, Point, Line, LineLike, LineSegment }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}

object Alpha_1_4_CircleInSquare {
    def problem[T](
        implicit constructible: ConstructibleTrait[T],
        ordering: Ordering[T]
    ): Problem[T] = {
        implicit val nToT = constructible.fromInt _
        val p1: Point[T] = Point(0, 0)
        val p2: Point[T] = Point(2, 0)
        val p3: Point[T] = Point(2, 2)
        val p4: Point[T] = Point(0, 2)
        val l1: LineLike[T] = LineSegment(p1, p2)
        val l2: LineLike[T] = LineSegment(p2, p3)
        val l3: LineLike[T] = LineSegment(p3, p4)
        val l4: LineLike[T] = LineSegment(p4, p1)
        val initialEnvironment = ProblemEnvironment.initialEnvironment[T](
            Board(Set(p1, p2, p3, p4), Set(l1, l2, l3, l4))
        )

        val goal: Circle[T] = Circle(Point(1, 1), 1)
        val answer = new FullFillEnvironmentAnswer[T](
            Vector(Board(circles = Set(goal))),
            Cost(3, 5),
        )

        val commands: Vector[CommandProvider[T]] = Vector(
            VerticalBisectorCommandProvider(),
            LineCommandProvider(),
            CircleCommandProvider(),
        )

        new Problem[T] {
            val problemL = ProblemL(initialEnvironment, commands, answer)
            val problemE = ProblemE(initialEnvironment, commands, answer)
        }
    }
}
