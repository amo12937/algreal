package amo.geometry.commands

import amo.geometry.problems.{ Cost, Board }

trait CommandProvider[T] {
    val cost: Cost
    def provideCommands(board: Board[T]): Iterator[Command[T]]
}

