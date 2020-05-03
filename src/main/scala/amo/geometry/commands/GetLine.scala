package amo.geometry.commands

import amo.algreal.field.ConstructibleTrait
import amo.geometry.figures.{ Line, Point }
import amo.geometry.problems.{ Board, Cost }

class LineCommand[T](p1: Point[T], p2: Point[T])(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends LineCommandBase[T] {
    val fig = Line(p1, p2)
    override def toString = s"Draw a line connecting point ${p1} and point ${p2}"
}

case class GetLineCommandProvider[T]()(
    implicit constructible: ConstructibleTrait[T],
    ordering: Ordering[T]
) extends CommandProvider[T] {
    val cost = Cost(1, 1)

    def provideCommands(board: Board[T]): Iterator[Command[T]] =
        board.points.toVector.combinations(2).map {
            case Seq(p1, p2) => new LineCommand(p1, p2)
        }
}
