import fetchInput from "../fetchinput";

const input = await fetchInput("2");

const games = input.split("\n").filter((line) => line !== "");

const regex = /^Game (\d+): (.*)$/;

let sum = 0;
const limit: { [key: string]: number } = { red: 12, green: 13, blue: 14 };
for (const game of games) {
  const [_, gameNumber, gameData] = game.match(regex)!;
  const runs = gameData.split(/[,;]/).map((run) => run.trim().split(" "));
  for (const run of runs) {
    if (limit[run[1]] < parseInt(run[0])) {
      console.log(game);
      console.log("Game", gameNumber, "is invalid, too many " + run[1] + "s");
      sum += parseInt(gameNumber);
      break;
    }
  }
}

console.log("Part1:", 5050 - sum);

for (const game of games) {
  const [_, gameNumber, gameData] = game.match(regex)!;
  const runs = gameData.split(/[,;]/).map((run) => run.trim().split(" "));
  for (const run of runs) {
    if (limit[run[1]] < parseInt(run[0])) {
      console.log(game);
      console.log("Game", gameNumber, "is invalid, too many " + run[1] + "s");
      sum += parseInt(gameNumber);
      break;
    }
  }
}
