import { Interface } from "node:readline";

export const questionFfi = (prompt) => (onError, onSuccess) => {
  const rl = new Interface({
    input: process.stdin,
    output: process.stdout,
  });

  rl.question(prompt, (answer) => {
    rl.close();
    onSuccess(answer);
  });

  return (cancelError, cancelerError, cancelerSuccess) => {
    rl.close();
    cancelerSuccess();
  };
};
