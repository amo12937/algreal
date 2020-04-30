package amo.geometry.commands

import amo.geometry.problems.Board

trait CommandProvider[T] {
    def provideCommands(board: Board[T]): Iterator[Command[T]]
}

