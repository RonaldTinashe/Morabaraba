module Morabaraba.Execution

val initialGame: Game

val execute: Game -> Action -> option<Game>
