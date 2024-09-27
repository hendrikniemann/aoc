import fetchInput from "../fetchinput";

const input = await fetchInput("4");

const cards = input
  .split("\n")
  .filter((line) => line !== "")
  .map((line) => {
    const [winning, have] = line.split(":")[1].trim().split("|");
    return {
      winning: winning.trim().split(/\s+/),
      have: have.trim().split(/\s+/),
    };
  });

const points = cards
  .map((card) => {
    const winning = new Set(card.winning);
    const count = card.have.filter((card) => winning.has(card)).length;
    if (count === 0) {
      return 0;
    }
    return 2 ** (count - 1);
  })
  .reduce((a, b) => a + b, 0);

console.log("Part 1: " + points);

let numCards = 0;
let stack: number[] = [];
for (const card of cards) {
  const winning = new Set(card.winning);
  const count = card.have.filter((card) => winning.has(card)).length;
  console.log(count, stack, numCards);
  const copies = stack.shift() ?? 0;
  for (let i = 0; i < count; i++) {
    stack[i] = (stack[i] ?? 0) + copies + 1;
  }
  numCards += copies + 1;
}

console.log("Part 2: " + numCards);
