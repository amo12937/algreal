package amo.geometry.problems

import amo.geometry.commands.Command
import amo.geometry.figures.{ LineLike, Point }

trait ProblemEnvironment[T] {
    val costL: Int
    val costE: Int

    val points: Set[Point[T]]
    val lines: Set[LineLike[T]]
    def commands: Vector[Command[T]]

    def addCommand(command: Command[T]): ProblemEnvironment[T]
    def addLine(_costL: Int, _costR: Int, line: LineLike[T]): ProblemEnvironment[T]

    // def addCircle
    // def addPoints
}
