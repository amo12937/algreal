package amo.geometry.problems

import scala.annotation.tailrec

import amo.geometry.commands.Command

class ProblemSolver[T] {
    def solve(problem: Problem[T]): Vector[Command[T]] = {
        val env = problem.initialEnvironment
        val queue = nextCommands(problem, env)
        recursiveSolve(problem, queue)
    }

    @tailrec
    final def recursiveSolve(
        problem: Problem[T],
        queue: Iterator[(Command[T], ProblemEnvironment[T])]
    ): Vector[Command[T]] = if (!queue.hasNext) Vector.empty else {
        val (command, prevProblemEnvironment) = queue.next
        val problemEnvironment = prevProblemEnvironment.applyCommand(command)
        if (problem.isSolved(problemEnvironment)) problemEnvironment.commands
        else {
            val nextQueue =
                if (problem.isOverCost(problemEnvironment.cost)) Iterator.empty
                else nextCommands(problem, problemEnvironment)
            recursiveSolve(problem, queue ++ nextQueue)
        }
    }

    def nextCommands(
        problem: Problem[T],
        problemEnvironment: ProblemEnvironment[T]
    ): Iterator[(Command[T], ProblemEnvironment[T])] = for {
        commandProvider <- problem.availableCommandProviders
        command <- commandProvider.provideCommands(problemEnvironment.board)
    } yield (command, problemEnvironment)
}
