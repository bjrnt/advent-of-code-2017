open Utils;

[@bs.deriving jsConverter]
type direction = [ | `NW | `N | `NE | `SW | `S | `SE];

let step = ((x, y, z), direction) =>
  switch direction {
  | `NW => (x - 1, y + 1, z)
  | `N => (x, y + 1, z - 1)
  | `NE => (x + 1, y, z - 1)
  | `SW => (x - 1, y, z + 1)
  | `S => (x, y - 1, z + 1)
  | `SE => (x + 1, y - 1, z)
  };

let directionOfString = directionFromJs >> expect("Unrecognized direction");

let origin = (0, 0, 0);

let distance = ((x1, y1, z1), (x2, y2, z2)) => (abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)) / 2;

let distanceFromOrigin = distance(origin);

let walk = (~onStep=step, origin, steps) => List.fold_left(onStep, origin, steps);

let stepsOfString = (str) => Utils.splitString(",", str) |> List.map(directionOfString);

let farthestDistance = (steps) => {
  let farthestDistance = ref(0);
  let recordDistance = (pos) => {
    let d = distanceFromOrigin(pos);
    if (d > farthestDistance^) {
      farthestDistance := d
    };
    pos
  };
  let _ = walk(~onStep=(pos) => step(pos) >> recordDistance, origin, stepsOfString(steps));
  farthestDistance^
};

let finalDistance = (steps) => distanceFromOrigin(walk(origin, stepsOfString(steps)));

/* Js.log(finalDistance(Inputs.day11)); */

/* Js.log(farthestDistance(Inputs.day11)); */
