import fetchInput from "../fetchinput";

const input = await fetchInput("5");

const cards = input.split("\n\n").map((line) => line.trim());

const seeds = cards.shift()?.split(":")[1].trim().split(" ") ?? [];

const rules = cards.map((line) =>
  line
    .split("\n")
    .slice(1)
    .map((line) =>
      line
        .trim()
        .split(" ")
        .map((x) => parseInt(x))
    )
    .map(([destination, source, range]) => ({ destination, source, range }))
);

const results = seeds.map((seed) => {
  let num = parseInt(seed);
  r: for (const rule of rules) {
    for (const row of rule) {
      if (num >= row.source && num < row.source + row.range) {
        num = row.destination + (num - row.source);
        continue r;
      }
    }
    console.log("No rule for", num);
  }
  return num;
});

console.log(results);

console.log(results.reduce((a, b) => Math.min(a, b), Number.POSITIVE_INFINITY));
