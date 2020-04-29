package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Circle, Point }
import amo.geometry.problems.ProblemEnvironment

case class GetCircleCommand[T](
    val p1: Point[T],
    val p2: Point[T]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val costL = 1
    val costE = 1

    def run(problemEnvironment: ProblemEnvironment[T]): ProblemEnvironment[T] =
        problemEnvironment
            .addCommand(this)
            .addCircle(Circle(p1, p1.dist(p2)))
            .addCost(costL, costE)
}

case class GetCircleCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    def provideCommands(problemEnvironment: ProblemEnvironment[T]): Iterator[Command[T]] =
        problemEnvironment.points.toVector.combinations(2).flatMap {
            case Seq(p1, p2) => Iterator(
                GetCircleCommand(p1, p2),
                GetCircleCommand(p2, p1),
            )
        }
}
