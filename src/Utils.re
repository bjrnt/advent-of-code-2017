let splitString = (sep, str) : list(string) => Js_string.split(sep, str) |> Array.to_list;

let linesOfString = splitString("\n");

let wordsOfString = splitString(" ");

let charsOfString = (s) => {
  let rec exp = (i, l) => i < 0 ? l : exp(i - 1, [s.[i], ...l]);
  exp(String.length(s) - 1, [])
};

let mod64 = (x: int64, y: int64) => {
  let x = ref(x);
  while (x^ > y) {
    x := Int64.sub(x^, y)
  };
  x^
};

let stringOfChars = (chars: list(char)) : string =>
  List.map(String.make(1), chars) |> String.concat("");

let zip2 = (xs: list('x), ys: list('y)) : list(('x, 'y)) =>
  List.fold_left2((list, x, y) => [(x, y), ...list], [], xs, ys);

/** Gives a list of numbers from 0..until inclusive */
let range = (until: int) : list(int) => {
  let rec makeRange = (list, next) => next > until ? list : makeRange([next, ...list], next + 1);
  List.rev(makeRange([], 0))
};

let isSome =
  fun
  | Some(_) => true
  | None => false;

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

let decToBin: int => string = [%bs.raw
  {|
function(s) {
  let result = s.toString(2);
  while(result.length < 32) {
    result = "0" + result;
  }
  return result;
}
|}
];

let loadInput: string => string = [%bs.raw
  {|
  function (filename) {
    return require('fs').readFileSync(`${__dirname}/inputs/${filename}.txt`).toString();
  }
|}
];

let rec indexOf = (e, xs) =>
  switch xs {
  | [] => raise(Failure("Couldn't find element"))
  | [x, ...xs] => x === e ? 0 : 1 + indexOf(e, xs)
  };
