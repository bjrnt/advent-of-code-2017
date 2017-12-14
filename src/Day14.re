open Utils;

module PSet =
  Set.Make(
    {
      let compare = Pervasives.compare;
      type t = (int, int);
    }
  );

let hash = Day10.hash;

let formatRow = (input, row) => input ++ "-" ++ string_of_int(row);

let hexToBin: char => string = [%bs.raw
  {|
  function(s) {
    let result = parseInt(String.fromCharCode(s),16).toString(2);
    while(result.length < 4) {
      result = "0" + result;
    }
    return result;
  }
|}
];

let countOnes = (str) => charsOfString(str) |> List.filter((==)('1')) |> List.length;

let exploreFrom = (start, matrix) => {
  /* Assumes the matrix is square */
  let dim = Array.length(matrix);
  let isValidNeighbor = ((x, y)) => x >= 0 && y >= 0 && y < dim && x < dim && matrix[x][y] === '1';
  /* No diagonals */
  let getNeighbors = ((x, y)) =>
    List.filter(isValidNeighbor, [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]);
  let rec walk = (visited, queue) =>
    switch queue {
    | [p, ...ps] when ! PSet.mem(p, visited) => walk(PSet.add(p, visited), ps @ getNeighbors(p))
    | [p] when ! PSet.mem(p, visited) => walk(PSet.add(p, visited), [])
    | [_, ...ps] => walk(visited, ps)
    | [] => visited
    };
  walk(PSet.empty, [start])
};

let _ = {
  let input = "nbysizxe";
  /* let input = "flqrgnkx"; */
  /* Part 1 */
  let bitStrings =
    List.map(
      (row) =>
        formatRow(input, row) |> hash |> charsOfString |> List.map(hexToBin) |> String.concat(""),
      range(127)
    );
  /* List.fold_left((bitsUsed, bitString) => bitsUsed + countOnes(bitString), 0, bitStrings) |> Js.log; */
  /* Part 2 */
  let onesPositions = ref(PSet.empty);
  let matrix = Array.make_matrix(128, 128, '0');
  List.iteri(
    (x, row) =>
      List.iteri(
        (y, bit) =>
          if (bit === '1') {
            matrix[x][y] = bit;
            onesPositions := PSet.add((x, y), onesPositions^)
          },
        charsOfString(row)
      ),
    bitStrings
  );
  let rec findGroups = (groups, visited) =>
    if (! PSet.equal(visited, onesPositions^)) {
      let unvisitedPosition = PSet.diff(onesPositions^, visited) |> PSet.choose;
      let group = exploreFrom(unvisitedPosition, matrix);
      findGroups([group, ...groups], PSet.union(group, visited))
    } else {
      groups
    };
  let groups = findGroups([], PSet.empty);
  Js.log(List.length(groups))
};
