open Utils;

/*
open Core

type state = {
  current_position:int;
  skip_size: int;
}

let reverse_slice array start len =
  let length = Array.length array in
  for i = 0 to (len / 2) - 1 do
    let j = (start + i) % length in
    let k = (start + len - 1 - i) % length in
    Array.swap array j k;
  done

let knot_hash array state length =
  let current_position = state.current_position in
  reverse_slice array current_position length;
  let array_length = Array.length array in
  let skip = state.skip_size in
  {
    current_position = (current_position + length + skip) % array_length;
    skip_size = skip + 1
  }

let hash init array lengths =
  let f = knot_hash array in
  List.fold lengths ~init ~f

let create_sparse_hash_array () =
  let array = Array.create ~len:256 0 in
  for i = 0 to 255 do
    array.(i) <- i
  done;
  array

let create_sparse_hash input =
  let sparse_hash = create_sparse_hash_array () in
  let rec loop state n =
    if n = 0 then sparse_hash
    else loop (hash state sparse_hash input) (n-1)
  in loop {current_position = 0; skip_size = 0;} 64

let create_dense_hash sparse =
  let dense = Array.create ~len:16 0 in
  for i = 0 to 255 do
    let j = i / 16 in
    dense.(j) <- Int.bit_xor dense.(j) sparse.(i);
  done;
  dense

let read_input () =
  let additional_lengths = In_channel.read_all "./input.txt"
                          |> String.to_list
                          |> List.map ~f:Char.to_int in
  List.append additional_lengths [17; 31; 73; 47; 23]

let _ =
  let hash = read_input ()
            |> create_sparse_hash
            |> create_dense_hash
            |> Array.map ~f:(sprintf "%02x")
            |> Array.to_list
            |> String.concat in
  printf "hash: %s\n" hash
*/



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

/* let part1 = {
     let (state,_,_) = runProg("192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12");
     Js.log(List.hd(state) * List.hd(List.tl(state)));
   }; */
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

/* let part2 = {
  /* let input = "1,2,4"; */
  let input = "192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12";
  let convertedInput = toAsciiCodes(input);
  let finalInput = convertedInput @ inputSuffix;
  let state = ref((makeList(255), 0, 0));
  for (_ in 1 to 64) {
    let (nz, currPos, skipLength) = performOps(state^, finalInput);
    state := (nz, currPos, skipLength)
  };
  let (nz, _, _) = state^;
  let sparseHash = Array.of_list(nz);
  let denseHash = Array.make(16, 0);
  for (block in 0 to 15) {
    let cb = Array.sub(sparseHash, block * 16, 16);
    denseHash[block] = cb[0];
    for (c in 1 to 15) {
      denseHash[block] = denseHash[block] lxor cb[c]
    }
  };
  let hash = List.fold_left((str, i) => str ++ toHex(i), "", Array.to_list(denseHash));
  Js.log(hash)

}; */
