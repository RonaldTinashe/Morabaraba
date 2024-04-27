module Morabaraba.Execution

val initialBoard: Board

val execute: Board -> Action -> option<Board>
