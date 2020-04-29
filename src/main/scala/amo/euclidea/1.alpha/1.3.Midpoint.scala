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
import amo.geometry.figures.{ Point }
import amo.geometry.problems.{
    FullFillEnvironmentAnswer, ProblemE, ProblemL, SimpleProblemEnvironment
}
import amo.implicits._

object Alpha_1_3_Midpoint {
    val p1: Point[AlgReal] = Point(0, 0)
    val p2: Point[AlgReal] = Point(2, 0)
    val initialEnvironment = SimpleProblemEnvironment[AlgReal](
        0, 0, Set(p1, p2)
    )

    val goal: Point[AlgReal] = Point(1, 0)
    val answer = new FullFillEnvironmentAnswer[AlgReal](SimpleProblemEnvironment(
        2, 4, Set(goal)
    ))

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

