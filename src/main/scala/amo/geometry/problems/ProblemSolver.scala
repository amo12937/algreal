package amo.geometry.problems

import scala.annotation.tailrec

import amo.geometry.commands.Command

class ProblemSolver[T] {
    def solve(problem: Problem[T]): Vector[Command[T]] = {
        val env = problem.initialProblemEnvironment
        val queue = nextCommands(problem, env)
        recursiveSolve(problem, queue)
    }

    @tailrec
    final def recursiveSolve(
        problem: Problem[T],
        queue: Iterator[(ProblemEnvironment[T], Command[T])]
    ): Vector[Command[T]] = if (!queue.hasNext) Vector.empty else {
        val (prevProblemEnvironment, command) = queue.next
        val problemEnvironment = command.run(prevProblemEnvironment)
        if (problem.isSolved(problemEnvironment)) problemEnvironment.commands
        else {
            val nextQueue =
                if (
                    problem.isOverCost(problemEnvironment)
                ) Iterator.empty
                else nextCommands(problem, problemEnvironment)
            recursiveSolve(problem, queue ++ nextQueue)
        }
    }

    def nextCommands(
        problem: Problem[T],
        problemEnvironment: ProblemEnvironment[T]
    ): Iterator[(ProblemEnvironment[T], Command[T])] = for {
        commandProvider <- problem.availableCommandProviders
        command <- commandProvider.provideCommands(problemEnvironment)
    } yield (problemEnvironment, command)
}
