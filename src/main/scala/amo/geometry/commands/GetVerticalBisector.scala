package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Line, Point }
import amo.geometry.problems.ProblemEnvironment

case class GetVerticalBisectorCommand[T](
    val p1: Point[T],
    val p2: Point[T]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val costL = 1
    val costE = 3

    def run(problemEnvironment: ProblemEnvironment[T]): ProblemEnvironment[T] = {
        val middle = (p1 + p2).scalarDiv(constructible.fromInt(2))
        val p3 = p2 - p1
        val p4 = middle + Point(constructible.negate(p3.y), p3.x)

        problemEnvironment
            .addCommand(this)
            .addLine(Line(middle, p4))
            .addCost(costL, costE)
    }
}

case class GetVerticalBisectorCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    def provideCommands(problemEnvironment: ProblemEnvironment[T]): Iterator[Command[T]] =
        problemEnvironment.points.toVector.combinations(2).map {
            case Seq(p1, p2) => GetVerticalBisectorCommand(p1, p2)
        }
}
