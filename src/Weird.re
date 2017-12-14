type direction =
  | NW
  | N
  | NE
  | SW
  | S
  | SE;

let directionOfString = (str) =>
  switch (str) {
  | "nw" => NW
  | "n" => N
  | "ne" => NE
  | "sw" => SW
  | "s" => S
  | "se" => SE
  | _ => failwith("Unrecognized direction: " ++ str)
  };


let stepsOfString = (str) => Js_string.split(",", str) |> Array.to_list |> List.map(directionOfString);

Js.log(stepsOfString(Inputs.day11));
