package amo.geometry.commands

import amo.algreal.field.ConstructibleTrait
import amo.geometry.figures.{ Line, Point }
import amo.geometry.problems.{ Board, Cost }

class VerticalBisectorCommand[T](p1: Point[T], p2: Point[T])(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends LineCommandBase[T] {
    val fig = {
        val middle = (p1 + p2).scalarDiv(constructible.fromInt(2))
        val p3 = p2 - p1
        val p4 = middle + Point(constructible.negate(p3.y), p3.x)
        Line(middle, p4)
    }
    override def toString = s"Draw a vertical bisector between point ${p1} and ${p2}"
}

case class GetVerticalBisectorCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = Cost(1, 3)

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).map {
            case Seq(p1, p2) => new VerticalBisectorCommand(p1, p2)
        }
}
