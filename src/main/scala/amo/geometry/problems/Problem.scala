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
    def canReachAnswer(
        problemEnvironment: ProblemEnvironment[T]
    ): Boolean =
        answer.remaining(problemEnvironment.board) <= remainingCost(problemEnvironment.cost)

    def costToInt(cost: Cost): Int
    def remainingCost(cost: Cost): Int =
        costToInt(answer.cost) - costToInt(cost)
    def isOverCost(cost: Cost): Boolean =
        remainingCost(cost) < 0

    def availableCommandProviders: Iterator[CommandProvider[T]] =
        commandProviders.toIterator
}

case class ProblemL[T](
    initialEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def costToInt(cost: Cost): Int = cost.costL
}

case class ProblemE[T](
    initialEnvironment: ProblemEnvironment[T],
    commandProviders: Vector[CommandProvider[T]],
    answer: ProblemAnswer[T]
) extends Problem[T] {
    def costToInt(cost: Cost): Int = cost.costE
}
