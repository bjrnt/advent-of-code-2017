open Utils;
let input = Inputs.day4;

module StringSet = Set.Make(String);

/* Part 1 */
let isValidPassword = (str: string) : bool => {
  open StringSet;
  let words = wordsOfString(str);
  List.length(of_list(words) |> elements) === List.length(words)
};

let countValidPasswords = (validator: string => bool, lines: string) : int =>
  List.fold_left(
    (count, line) => count + (validator(line) ? 1 : 0),
    0,
    linesOfString(lines)
  );

/* Js.log(countValidPasswords(input, isValidPassword)); */
/* Part 2 */
let isValidAnagramPassword = (str: string) : bool => {
  open StringSet;
  let charSortedWords =
    List.map(
      (word) => charsToString(List.sort(Char.compare, charsOfString(word))),
      wordsOfString(str)
    );
  List.length(of_list(charSortedWords) |> elements) === List.length(charSortedWords)
};

/* Js.log(countValidPasswords(input, isValidAnagramPassword)); */
