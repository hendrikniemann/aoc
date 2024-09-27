import fetchInput from "./fetchinput";
import * as day1 from "./advent/day1";

const input = await fetchInput("1");

console.log(day1.solveExtra(input));
