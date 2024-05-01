# Morabaraba

## Rules
The game follows the generally accepted guidelines
set by [Mind Sports South Africa](https://esportscommentator.blogspot.com/2021/05/generally-accepted-rules-gar-for.html).

## Run

To run the game from the terminal use the following commands.

```sh
mkdir play && cd play
git clone https://github.com/RonaldTinashe/Morabaraba
cd Morabaraba
dotnet run --project Morabaraba.Console
```

## Playing

When prompted for a move, you may either enter a single junction or two junctions.

A single junction represents either a placement or a shot. An example of this is `E4`.

If it is a placement, the turn would be fresh. If it is a shot, the player would have made a move prior
to the current prompt.

Two junctions represent movement from one junction to another. An example of a movement is `A1A7`.
