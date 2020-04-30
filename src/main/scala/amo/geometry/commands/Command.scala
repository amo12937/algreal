package amo.geometry.commands

import amo.geometry.problems.{ Cost, Board }

trait Command[T] {
    val cost: Cost
    def run(board: Board[T]): Board[T]
}
