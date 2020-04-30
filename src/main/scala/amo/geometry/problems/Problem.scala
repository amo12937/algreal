package amo.geometry.problems

import amo.geometry.commands.CommandProvider

trait Problem[T] {
    val initialEnvironment: ProblemEnvironment[T]
    val commandProviders: Vector[CommandProvider[T]]
    val answer: ProblemAnswer[T]

    def isSolved(
        problemEnvironment: ProblemEnvironment[T]
    ): Boolean =
        !isOverCost(problemEnvironment.cost) && answer.fulfill(problemEnvironment.board)
    def isOverCost(cost: Cost): Boolean
    def availableCommandProviders: Iterator[CommandProvider[T]] =
        commandProviders.toIterator
}

case class ProblemL[T](
    initialEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def isOverCost(cost: Cost): Boolean = answer.cost.costL < cost.costL
}

case class ProblemE[T](
    initialEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def isOverCost(cost: Cost): Boolean = answer.cost.costE < cost.costE
}
