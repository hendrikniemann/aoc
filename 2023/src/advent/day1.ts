export function solveExtra(input: string) {
  const lines = input.split("\n");
  const numbers = lines
    .filter((line) => line !== "")
    .map((line) => {
      const forward = line.match(regexForward);
      const backward = line.split("").reverse().join("").match(regexBackward);
      if (!forward || !backward) {
        throw new Error("No match found: " + line);
      }
      if (!assignments.has(forward[0]) || !assignments.has(backward[0])) {
        throw new Error("Match not in list found");
      }
      return parseInt(
        assignments.get(forward[0])! + assignments.get(backward[0])!
      );
    });
  const sum = numbers.reduce((acc, curr) => acc + curr, 0);
  return sum;
}

const regexForward = new RegExp(
  "([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)"
);
const regexBackward = new RegExp(
  "([0-9]|orez|enin|thgie|neves|xis|evif|ruof|eerht|owt|eno)"
);

const assignments = new Map<string, string>([
  ["1", "1"],
  ["one", "1"],
  ["eno", "1"],
  ["2", "2"],
  ["two", "2"],
  ["owt", "2"],
  ["3", "3"],
  ["three", "3"],
  ["eerht", "3"],
  ["4", "4"],
  ["four", "4"],
  ["ruof", "4"],
  ["5", "5"],
  ["five", "5"],
  ["evif", "5"],
  ["6", "6"],
  ["six", "6"],
  ["xis", "6"],
  ["7", "7"],
  ["seven", "7"],
  ["neves", "7"],
  ["8", "8"],
  ["eight", "8"],
  ["thgie", "8"],
  ["9", "9"],
  ["nine", "9"],
  ["enin", "9"],
  ["0", "0"],
  ["zero", "0"],
  ["orez", "0"],
]);
