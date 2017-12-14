open Utils;

let logList = (node) => Js.log(Array.of_list(node));

let parseOps = (str) => splitString(",", str) |> List.map(int_of_string);

let rec rotateLeft = (n, xs) =>
  switch n {
  | 0 => xs
  | _ => rotateLeft(n - 1, List.tl(xs) @ [List.hd(xs)])
  };

let reverseSlice = (nums, start, len) => {
  let length = Array.length(nums);
  for (i in 0 to (len / 2) - 1) {
    let j = (start + i) mod length;
    let k = (start + len - 1 - i) mod length;
    let tmp = nums[k];
    nums[k] = nums[j];
    nums[j] = tmp;
  }
};

let performOp = ((nums, currPos, skipLength), op: int) => {
  reverseSlice(nums, currPos, op);
  (nums, (currPos + op + skipLength) mod Array.length(nums), skipLength + 1)
};

let performOps = (state, ops: list(int)) => List.fold_left(performOp, state, ops);

let makeArray = (n) =>
  Array.init(n, n => n);

let runProg = (str: string) => performOps((makeArray(256), 0, 0), parseOps(str));

let toAsciiCodes = (str: string) => List.map(Char.code, charsOfString(str));

let inputSuffix = [17, 31, 73, 47, 23];

let toHex = (i) => {
  let s = Js_int.toStringWithRadix(i, ~radix=16);
  String.length(s) === 1 ? "0" ++ s : s
};

let hash = (input: string) => {
  let input = toAsciiCodes(input) @ inputSuffix;
  let (sparseHash, _, _) = List.fold_left((state, _) => performOps(state, input), (makeArray(256), 0, 0), range(63));
  let denseHash = Array.make(16, 0);
  for (i in 0 to 255) {
    let j = i / 16;
    denseHash[j] = denseHash[j] lxor sparseHash[i];
  };
  let hash = Array.fold_left((str, i) => str ++ toHex(i), "", denseHash);
  hash
};

