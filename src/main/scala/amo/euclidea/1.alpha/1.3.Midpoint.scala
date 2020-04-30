package amo.euclidea.alpha

import amo.algreal.AlgReal
import amo.euclidea.Problem
import amo.geometry.commands.{
    CommandProvider,
    GetCircleCommandProvider,
    GetLineCommandProvider,
    GetVerticalBisectorCommandProvider
}
import amo.geometry.figures.{ Point }
import amo.geometry.problems.{
    Board,
    Cost,
    FullFillEnvironmentAnswer,
    ProblemE,
    ProblemL,
    ProblemEnvironment
}
import amo.implicits._

object Alpha_1_3_Midpoint {
    val p1: Point[AlgReal] = Point(0, 0)
    val p2: Point[AlgReal] = Point(2, 0)
    val initialEnvironment = ProblemEnvironment.initialEnvironment[AlgReal](
        Board(Set(p1, p2))
    )

    val goal: Point[AlgReal] = Point(1, 0)
    val answer = new FullFillEnvironmentAnswer[AlgReal](
        Board(Set(goal)),
        Cost(2, 4)
    )

    val commands: Vector[CommandProvider[AlgReal]] = Vector(
        GetLineCommandProvider(),
        GetCircleCommandProvider(),
        GetVerticalBisectorCommandProvider()
    )

    val problem = new Problem[AlgReal] {
        val problemL = ProblemL(initialEnvironment, commands, answer)
        val problemE = ProblemE(initialEnvironment, commands, answer)
    }
}

