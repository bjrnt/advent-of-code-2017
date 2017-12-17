open Utils;

type move =
  | Spin(int)
  | Exchange(int, int)
  | Partner(char, char);

let parseMove = (str) =>
  switch (charsOfString(str)) {
  | ['s', ...n] => Spin(int_of_string(stringOfChars(n)))
  | ['x', ...n] =>
    let [a, b] = stringOfChars(n) |> splitString("/");
    Exchange(int_of_string(a), int_of_string(b))
  | ['p', a, _, b] => Partner(a, b)
  | _ => failwith("Unrecognized move: " ++ str)
  };

let indexOf = (array, target) => {
  let pos = ref((-1));
  Array.iteri(
    (i, elem) =>
      if (elem === target) {
        pos := i
      },
    array
  );
  pos^
};

let swap = (array, x, y) => {
  let tmp = array[x];
  array[x] = array[y];
  array[y] = tmp;
  array
};

let parseMoves = (str) => List.map(parseMove, splitString(",", str));

let startingPositions = Array.init(16, (i) => char_of_int(int_of_char('a') + i));

let len = Array.length(startingPositions);

let performMove = (positions, move) =>
  switch move {
  | Spin(n) =>
    let cpy = Array.copy(positions);
    let len = len - n;
    Array.blit(cpy, len, positions, 0, n);
    Array.blit(cpy, 0, positions, n, len);
    positions
  | Exchange(x, y) => swap(positions, x, y)
  | Partner(a, b) =>
    let (indexA, indexB) = (indexOf(positions, a), indexOf(positions, b));
    swap(positions, indexA, indexB)
  };

let dance = (positions, moves) => List.fold_left(performMove, positions, moves);

let stringOfPositions = (pos) => Array.to_list(pos) |> stringOfChars;

let _ = {
  let moves = loadInput("day16") |> parseMoves;
  /* Part 1 */
  /* Js.log(dance(startingPositions, moves) |> Array.to_list |> stringOfChars); */
  /* Part 2 */
  let rec danceDanceDance = (knownStarts, positions, i) =>
    switch (indexOf(Array.of_list(knownStarts), stringOfPositions(positions))) {
    | (-1) =>
      danceDanceDance(
        [stringOfPositions(positions), ...knownStarts],
        dance(Array.copy(positions), moves),
        i + 1
      )
    | _ => List.nth(List.rev(knownStarts), 1_000_000_000 mod i)
    };
  danceDanceDance([], startingPositions, 0) |> Js.log
};
