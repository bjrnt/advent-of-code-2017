open Utils;

let parse = (str) => {
  let chars = charsOfString(str);
  let rec parseGroup = (chars, depth, score, garbageCount) =>
    switch chars {
    | ['}'] => (score + depth, garbageCount)
    | ['}', ...tl] => parseGroup(tl, depth - 1, score + depth, garbageCount)
    | ['{', ...tl] => parseGroup(tl, depth + 1, score, garbageCount)
    | [',', ...tl] => parseGroup(tl, depth, score, garbageCount)
    | ['<', ...tl] => parseGarbage(tl, false, depth, score, garbageCount)
    | _ => failwith("Failed on" ++ stringOfChars(chars))
    }
  and parseGarbage = (chars, ignoreNext, depth, score, garbageCount) =>
    if (ignoreNext) {
      parseGarbage(List.tl(chars), false, depth, score, garbageCount)
    } else {
      switch chars {
      | ['>', ...tl] => parseGroup(tl, depth, score, garbageCount)
      | ['!', ...tl] => parseGarbage(tl, true, depth, score, garbageCount)
      | [_, ...tl] => parseGarbage(tl, false, depth, score, garbageCount + 1)
      | _ => failwith("Failed on" ++ stringOfChars(chars))
      }
    };
  parseGroup(List.tl(chars), 1, 0, 0)
};

let test = (str, expected, expectedGarbage) => {
  let (score, garbage) = parse(str);
  if (score != expected || garbage != expectedGarbage) {
    Js.log(str);
    Js.log("Score: " ++ string_of_int(score) ++ " == " ++ string_of_int(expected));
    Js.log("Garbage: " ++ string_of_int(garbage) ++ " == " ++ string_of_int(expectedGarbage))
  }
};
/* test("{}", 1, 0);

   test("{{}}", 3, 0);

   test("{{{}}}", 6, 0);

   test("{{},{}}", 5, 0);

   test("{{},{},{}}", 7, 0);

   test("{<a>,<a>,<a>,<a>}", 1, 4);

   test("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9, 8);

   test("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9, 0);

   test("{{{},{},{{}}}}", 16, 0);
   test("{<{o\"i!a,<{i<a>}", 1, 10);

   Js.log(parse(Inputs.day9));
   */
