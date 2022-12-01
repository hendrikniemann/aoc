import fetch from "node-fetch";

export const fetchInputImpl = (day) => (onError, onSuccess) => {
  fetch("https://adventofcode.com/2022/day/" + day + "/input", {
    method: "GET",
    headers: new fetch.Headers({
      Cookie: "session=" + process.env.ADVENT_OF_CODE_CREDENTIALS,
    }),
  })
    .then((resp) => resp.text())
    .then(onSuccess, onError);

  return (cancelError, cancelerError, cancelerSuccess) => {
    cancelerSuccess();
  };
};
