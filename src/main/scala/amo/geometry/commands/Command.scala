package amo.geometry.commands

import amo.geometry.figures.{ Circle, Figure2D, LineLike }
import amo.geometry.problems.{ Cost, Board }

trait Command[T] {
    def run(board: Board[T]): Board[T]
}

trait CommandWithFig[T, S <: Figure2D[T]] extends Command[T] with Equals {
    val fig: S

    def canEqual(rhs: Any) = rhs.isInstanceOf[CommandWithFig[T, S]]
    override def equals(rhs: Any) = rhs match {
        case r: CommandWithFig[T, S] => r.canEqual(this) && fig == r.fig
        case _ => false
    }
    override def hashCode = fig.hashCode
}

trait LineCommandBase[T] extends CommandWithFig[T, LineLike[T]] {
    val fig: LineLike[T]
    def run(board: Board[T]): Board[T] =
        board.addLine(fig)
}

trait CircleCommandBase[T] extends CommandWithFig[T, Circle[T]] {
    val fig: Circle[T]
    def run(board: Board[T]): Board[T] =
        board.addCircle(fig)
}
