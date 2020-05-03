package amo.geometry.commands

import amo.algreal.field.ConstructibleTrait
import amo.geometry.figures.{ Line, Point }
import amo.geometry.problems.{ Board, Cost }

case class GetVerticalBisectorCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = Cost(1, 3)

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).map {
            case Seq(p1, p2) => makeCommand(p1, p2)
        }

    def makeCommand(p1: Point[T], p2: Point[T]): Command[T] = {
        val middle = (p1 + p2).scalarDiv(constructible.fromInt(2))
        val p3 = p2 - p1
        val p4 = middle + Point(constructible.negate(p3.y), p3.x)
        LineCommand(Line(middle, p4))
    }
}
