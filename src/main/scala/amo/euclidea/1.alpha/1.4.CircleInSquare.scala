package amo.euclidea.alpha

import amo.algreal.AlgReal
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    GetCircleCommandProvider,
    GetLineCommandProvider,
    GetPointCommandProvider,
    GetVerticalBisectorCommandProvider
}
import amo.geometry.figures.{ Circle, Point, Line, LineLike, LineSegment }
import amo.geometry.problems.{
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    SimpleProblemEnvironment
}
import amo.implicits._

object Alpha_1_4_CircleInSquare {
    val p1: Point[AlgReal] = Point(0, 0)
    val p2: Point[AlgReal] = Point(2, 0)
    val p3: Point[AlgReal] = Point(2, 2)
    val p4: Point[AlgReal] = Point(0, 2)
    val l1: LineLike[AlgReal] = LineSegment(p1, p2)
    val l2: LineLike[AlgReal] = LineSegment(p2, p3)
    val l3: LineLike[AlgReal] = LineSegment(p3, p4)
    val l4: LineLike[AlgReal] = LineSegment(p4, p1)
    val initialEnvironment = SimpleProblemEnvironment[AlgReal](
        Set(p1, p2, p3, p4), Set(l1, l2, l3, l4)
    )

    val goal: Circle[AlgReal] = Circle(Point(1, 1), AlgReal(1))
    val answer = new FullFillEnvironmentAnswer[AlgReal](
        Cost(3, 5),
        SimpleProblemEnvironment(
            circles = Set(goal)
        )
    )

    val commands: Vector[CommandProvider[AlgReal]] = Vector(
        GetLineCommandProvider(),
        GetCircleCommandProvider(),
        GetPointCommandProvider(),
        GetVerticalBisectorCommandProvider()
    )

    val problem = new Problem[AlgReal] {
        val problemL = ProblemL(initialEnvironment, commands, answer)
        val problemE = ProblemE(initialEnvironment, commands, answer)
    }
}
