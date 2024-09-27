import fetchInput from "../fetchinput";

const input = await fetchInput("3");

const matrix = input.split("\n").map((line) => line.split(""));

const isDigit = (char = "") => /\d/.test(char);

const adjectedSymbol = (row: number, [start, end]: [number, number]) => {
  for (let i = row - 1; i <= row + 1; i++) {
    for (let j = start - 1; j <= end + 1; j++) {
      if (
        matrix[i]?.[j] &&
        matrix[i]?.[j] !== "." &&
        !isDigit(matrix[i]?.[j])
      ) {
        return true;
      }
    }
  }
  return false;
};

const numbers: number[] = [];

for (let row = 0; row < matrix.length; row++) {
  for (let column = 0; column < matrix[row].length; column++) {
    if (!isDigit(matrix[row][column])) continue;

    let end = column;
    while (isDigit(matrix[row][end + 1])) {
      end++;
    }

    if (adjectedSymbol(row, [column, end])) {
      const number = matrix[row].slice(column, end + 1).join("");
      numbers.push(parseInt(number));
    }

    // skip till end of current number
    column = end;
  }
}

console.log(numbers.reduce((a, b) => a + b, 0));

const adjectedStar = (row: number, [start, end]: [number, number]) => {
  for (let i = row - 1; i <= row + 1; i++) {
    for (let j = start - 1; j <= end + 1; j++) {
      if (matrix[i]?.[j] === "*") {
        return `${i}:${j}`;
      }
    }
  }
  return null;
};

const possibleGears: Map<string, number[]> = new Map();

for (let row = 0; row < matrix.length; row++) {
  for (let column = 0; column < matrix[row].length; column++) {
    if (!isDigit(matrix[row][column])) continue;

    let end = column;
    while (isDigit(matrix[row][end + 1])) {
      end++;
    }

    const star = adjectedStar(row, [column, end]);
    if (star) {
      const number = parseInt(matrix[row].slice(column, end + 1).join(""));
      const current = possibleGears.get(star) || [];
      possibleGears.set(star, [...current, number]);
    }

    // skip till end of current number
    column = end;
  }
}

const result = Array.from(possibleGears.values())
  .filter((g) => g.length === 2)
  .reduce((a, [x, y]) => a + x * y, 0);

console.log(result);
