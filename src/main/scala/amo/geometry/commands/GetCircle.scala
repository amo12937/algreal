package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.Circle
import amo.geometry.problems.{ Board, Cost }

case class GetCircleCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = Cost(1, 1)

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).flatMap {
            case Seq(p1, p2) => {
                val r = p1.dist(p2)
                Iterator(
                    CircleCommand(Circle(p1, r)),
                    CircleCommand(Circle(p2, r)),
                )
            }
        }
}
