package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Point, Line }
import amo.geometry.problems.{ Board, Cost }

case class GetLineCommand[T](
    val p1: Point[T],
    val p2: Point[T]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val cost = GetLineCommand.cost

    def run(board: Board[T]): Board[T] =
        board.addLine(Line(p1, p2))
}

case class GetLineCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = GetLineCommand.cost

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).map {
            case Seq(p1, p2) => GetLineCommand(p1, p2)
        }
}

object GetLineCommand {
    val cost = Cost(1, 1)
}
