let linesOfString = (str) => Js_string.split("\n", str) |> Array.to_list;

let splitString = (sep, str) : list(string) => Js_string.split(sep, str) |> Array.to_list;

let wordsOfString = splitString(" ");

let charsOfString = (str: string) : list(char) => {
  let rec exp = (i, l) => i < 0 ? l : exp(i - 1, [str.[i], ...l]);
  exp(String.length(str) - 1, [])
};

let charsToString = (chars: list(char)) : string =>
  List.map(String.make(1), chars) |> String.concat("");

let zip2 = (xs: list('x), ys: list('y)) : list(('x, 'y)) =>
  List.fold_left2((list, x, y) => [(x, y), ...list], [], xs, ys);

let range = (until: int) : list(int) => {
  let rec makeRange = (list, next) => next > until ? list : makeRange([next, ...list], next + 1);
  List.rev(makeRange([], 0))
};

let isSome =
  fun
  | Some(_) => true
  | None => false;

let rec take = (n, xs) =>
  n == 0 ?
    [] :
    (
      switch xs {
      | [x, ...xs] => [x, ...take(n - 1, xs)]
      | _ => failwith("Cannot take from list")
      }
    );

let rec skip = (n, xs) =>
  switch n {
  | 0 => xs
  | 1 => List.tl(xs)
  | _ => skip(n - 1, List.tl(xs))
  };

let expect = (str, x) =>
  switch x {
  | Some(x) => x
  | None => failwith(str)
  };

let (>>) = (f: 'a => 'b, g: 'b => 'c, x) => g(f(x));

let remember = (pred: ('a, 'a) => bool, reference: ref('a), nextValue: 'a) => {
  if (pred(nextValue, reference^)) {
    reference := nextValue
  };
  nextValue
};

/* Js.log(
  {
    let vals = [1, 2, 3, 4, 5];
    let smallest = ref(max_int);
    let largest = ref(min_int);
    let _ = List.map(remember((<), smallest) >> remember((>), largest), vals);
    (smallest^, largest^)
  }
); */
