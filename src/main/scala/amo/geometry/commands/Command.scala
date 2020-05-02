package amo.geometry.commands

import amo.geometry.figures.{ LineLike, Circle }
import amo.geometry.problems.{ Cost, Board }

trait Command[T] {
    def run(board: Board[T]): Board[T]
}

case class LineCommand[T](line: LineLike[T]) extends Command[T] {
    def run(board: Board[T]): Board[T] =
        board.addLine(line)
}

case class CircleCommand[T](circle: Circle[T]) extends Command[T] {
    def run(board: Board[T]): Board[T] =
        board.addCircle(circle)
}
