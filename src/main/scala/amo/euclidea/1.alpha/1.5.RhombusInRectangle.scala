package amo.euclidea.alpha

import amo.algreal.AlgReal
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    GetCircleCommandProvider,
    GetLineCommandProvider,
    GetVerticalBisectorCommandProvider
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
import amo.implicits._

object Alpha_1_5_RhombusInRectangle {
    val a = AlgReal(3).nthRoot(3) // 3^(1/3) ≒ 1.442
    val p1: Point[AlgReal] = Point(0, 0)
    val p2: Point[AlgReal] = Point(a, 0)
    val p3: Point[AlgReal] = Point(a, 1)
    val p4: Point[AlgReal] = Point(0, 1)
    val l1: LineLike[AlgReal] = LineSegment(p1, p2)
    val l2: LineLike[AlgReal] = LineSegment(p2, p3)
    val l3: LineLike[AlgReal] = LineSegment(p3, p4)
    val l4: LineLike[AlgReal] = LineSegment(p4, p1)

    val initialEnvironment = ProblemEnvironment.initialEnvironment[AlgReal](
        Board(Set(p1, p2, p3, p4), Set(l1, l2, l3, l4))
    )

    val q1 = ((a^2) - 1) / (2 * a)
    val q2 = a - q1

    val answer = new FullFillEnvironmentAnswer[AlgReal](
        Vector(
            Board(lines = Set(
                Line(p0, Point(q2, 1)),
                Line(Point(q1, 0), p3)
            )),
            Board(lines = Set(
                Line(p4, Point(q2, 0)),
                Line(Point(q1, 1), p2)
            ))
        ),
        Cost(3, 5)
    )

    val commands: Vector[CommandProvider[AlgReal]] = Vector(
        GetVerticalBisectorCommandProvider(),
        GetLineCommandProvider(),
        GetCircleCommandProvider(),
    )

    val problem = new Problem[AlgReal] {
        val problemL = ProblemL(initialEnvironment, commands, answer)
        val problemE = ProblemE(initialEnvironment, commands, answer)
    }
}