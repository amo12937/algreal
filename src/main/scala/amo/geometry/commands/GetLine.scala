package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.Line
import amo.geometry.problems.{ Board, Cost }

case class GetLineCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = Cost(1, 1)

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).map {
            case Seq(p1, p2) => LineCommand(Line(p1, p2))
        }
}
