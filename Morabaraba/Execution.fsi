module Morabaraba.Executor

val initialGame: Game

val execute: Game -> Action -> option<Game>
