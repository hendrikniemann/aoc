const input = await Bun.file(import.meta.dir + "/../inputs/day1.txt").text();

const rows = input.split("\n");

const list1 = rows.map((row) => parseInt(row.split(/\s+/)[0])).sort();
const list2 = rows.map((row) => parseInt(row.split(/\s+/)[1])).sort();

let sum = 0;

for (let i = 0; i < list1.length; i++) {
  sum += Math.abs(list1[i] - list2[i]);
}

console.log("Part1:", sum);

sum = 0;
for (let i = 0; i < list1.length; i++) {
  const count = list2.filter((x) => x === list1[i]).length;
  sum += count * list1[i];
}

console.log("Part2:", sum);
