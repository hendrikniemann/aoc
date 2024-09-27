export default function fetchInput(day: string) {
  return fetch("https://adventofcode.com/2023/day/" + day + "/input", {
    method: "GET",
    headers: new Headers({
      Cookie: "session=" + process.env.ADVENT_OF_CODE_CREDENTIALS,
    }),
  }).then((resp) => resp.text());
}
