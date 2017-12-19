open Utils;

type dir =
  | Up
  | Down
  | Left
  | Right;

type pos = {
  r: int,
  c: int
};

type vec = {
  pos,
  dir
};

let findEntryPoint = (pipes) => {
  pos: {r: 0, c: Array.to_list(pipes[0]) |> indexOf('|')},
  dir: Down
};

let isCheckpoint = (c) => Char.code(c) >= Char.code('A') && Char.code(c) <= Char.code('Z');

let withinBounds = (pipes, {pos: {r, c}}) =>
  r > 0 && r < Array.length(pipes) - 1 && c > 0 && c < Array.length(pipes[0]) - 1;

let turn = (pipes, {pos: {r, c}, dir}) => {
  let matchesDir = (ch, dir) =>
    isCheckpoint(ch) ?
      true :
      (
        switch dir {
        | Up
        | Down => ch === '|'
        | Left
        | Right => ch === '-'
        }
      );
  let turnValidDir = ({pos: {r, c}, dir} as posA, posB) =>
    withinBounds(pipes, posA) && matchesDir(pipes[r][c], dir) ? posA : posB;
  switch dir {
  | Up
  | Down => turnValidDir({pos: {r, c: c - 1}, dir: Left}, {pos: {r, c: c + 1}, dir: Right})
  | Left
  | Right => turnValidDir({pos: {r: r + 1, c}, dir: Down}, {pos: {r: r - 1, c}, dir: Up})
  }
};

let takeStep = ({pos: {r, c}, dir}) => {
  dir,
  pos:
    switch dir {
    | Up => {r: r - 1, c}
    | Down => {r: r + 1, c}
    | Left => {r, c: c - 1}
    | Right => {r, c: c + 1}
    }
};

let rec travel = (pipes, {pos: {r, c}} as vec, checkpoints, stepsTaken) =>
  switch pipes[r][c] {
  | ' ' => (checkpoints, stepsTaken - 1)
  | '+' => travel(pipes, turn(pipes, vec), checkpoints, stepsTaken + 1)
  | c when isCheckpoint(c) => travel(pipes, takeStep(vec), [c, ...checkpoints], stepsTaken + 1)
  | _ => travel(pipes, takeStep(vec), checkpoints, stepsTaken + 1)
  };

let _ = {
  /* let input = loadInput("day19_test"); */
  let input = loadInput("day19");
  let pipes =
    linesOfString(input)
    |> Array.of_list
    |> Array.map((line) => charsOfString(line) |> Array.of_list);
  let entryPoint = findEntryPoint(pipes);
  let (checkpoints, stepsTaken) = travel(pipes, entryPoint, [], 1);
  /* Part 1 */
  checkpoints |> List.rev |> stringOfChars |> Js.log2("Checkpoints:");
  /* Part 2 */
  stepsTaken |> Js.log2("Steps taken:")
};
