package amo.geometry.commands

import amo.algreal.Field.ConstructibleTrait
import amo.geometry.figures.{ Line, Point }
import amo.geometry.problems.{ Board, Cost }

case class GetVerticalBisectorCommand[T](
    val p1: Point[T],
    val p2: Point[T]
)(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends Command[T] {
    val cost = GetVerticalBisectorCommand.cost

    def run(board: Board[T]): Board[T] = {
        val middle = (p1 + p2).scalarDiv(constructible.fromInt(2))
        val p3 = p2 - p1
        val p4 = middle + Point(constructible.negate(p3.y), p3.x)

        board.addLine(Line(middle, p4))
    }
}

case class GetVerticalBisectorCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = GetVerticalBisectorCommand.cost

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).map {
            case Seq(p1, p2) => GetVerticalBisectorCommand(p1, p2)
        }
}

object GetVerticalBisectorCommand {
    val cost = Cost(1, 3)
}
