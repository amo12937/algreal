package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Circle, Point }
import amo.geometry.problems.{ Board, Cost }

case class GetCircleCommand[T](
    val p1: Point[T],
    val p2: Point[T]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val cost = GetCircleCommand.cost

    def run(board: Board[T]): Board[T] =
        board.addCircle(Circle(p1, p1.dist(p2)))
}

case class GetCircleCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = GetCircleCommand.cost

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).flatMap {
            case Seq(p1, p2) => Iterator(
                GetCircleCommand(p1, p2),
                GetCircleCommand(p2, p1),
            )
        }
}

object GetCircleCommand {
    val cost = Cost(1, 1)
}
