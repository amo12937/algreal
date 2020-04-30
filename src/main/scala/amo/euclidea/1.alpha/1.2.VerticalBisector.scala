package amo.euclidea.alpha

import amo.algreal.AlgReal
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    GetCircleCommandProvider,
    GetLineCommandProvider,
}
import amo.geometry.figures.{ Point, Line, LineLike, LineSegment }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}
import amo.implicits._

object Alpha_1_2_VerticalBisector {
    val p1: Point[AlgReal] = Point(0, 0)
    val p2: Point[AlgReal] = Point(2, 0)
    val l: LineLike[AlgReal] = LineSegment(p1, p2)
    val initialEnvironment = ProblemEnvironment.initialEnvironment[AlgReal](
        Board(Set(p1, p2), Set(l))
    )

    val goal: LineLike[AlgReal] = Line(Point(1, -1), Point(1, 1))
    val answer = new FullFillEnvironmentAnswer[AlgReal](
        Vector(Board(Set(), Set(goal))),
        Cost(3, 3)
    )

    val commands: Vector[CommandProvider[AlgReal]] = Vector(
        GetLineCommandProvider(),
        GetCircleCommandProvider()
    )

    val problem = new Problem[AlgReal] {
        val problemL = ProblemL(initialEnvironment, commands, answer)
        val problemE = ProblemE(initialEnvironment, commands, answer)
    }
}

