package amo.geometry.problems

import amo.geometry.commands.Command
import amo.geometry.figures.{ Circle, LineLike, Point }

case class ProblemEnvironment[T](
    board: Board[T],
    cost: Cost,
    commands: Vector[Command[T]],
) {
    def applyCommand(command: Command[T], _cost: Cost): ProblemEnvironment[T] =
        ProblemEnvironment(
            command.run(board),
            cost + _cost,
            commands :+ command
        )
}

object ProblemEnvironment {
    def initialEnvironment[T](board: Board[T]): ProblemEnvironment[T] =
        ProblemEnvironment(board, Cost(0, 0), Vector())
}
