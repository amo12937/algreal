package amo.geometry.commands

import amo.geometry.problems.ProblemEnvironment

trait CommandProvider[T] {
    def provideCommands(problemEnvironment: ProblemEnvironment[T]): Iterator[Command[T]]
}

