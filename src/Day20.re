open Utils;

type vector3d = {
  x: int,
  y: int,
  z: int
};

type particle = {
  pos: vector3d,
  vel: vector3d,
  acc: vector3d
};

let add = (v1, v2) => {x: v1.x + v2.x, y: v1.y + v2.y, z: v1.z + v2.z};

let collides = ({pos: p1}, {pos: p2}) => p1.x == p2.x && p1.y == p2.y && p1.z == p2.z;

let parseTuple = (re, str) =>
  switch (Js.String.match(re, str)) {
  | None => failwith("Could not match regex " ++ str)
  | Some(result) =>
    let [x, y, z] = result[1] |> splitString(",") |> List.map(int_of_string);
    {x, y, z}
  };

let parseParticle = (str) => {
  pos: parseTuple([%bs.re "/p=<([0-9\\-,]+)>/"], str),
  vel: parseTuple([%bs.re "/v=<([0-9\\-,]+)>/"], str),
  acc: parseTuple([%bs.re "/a=<([0-9\\-,]+)>/"], str)
};

let distanceFromOrigin = ({x, y, z}) => abs(x) + abs(y) + abs(z);

let updateParticle = ({pos, vel, acc}) => {
  let vel = add(vel, acc);
  let pos = add(pos, vel);
  {acc, pos, vel}
};

let _ = {
  let input = loadInput("day20");
  let particles = linesOfString(input) |> List.map(parseParticle);
  /* Part 1 */
  /* let closestToOrigin =
    List.fold_left((particles, _) => List.map(updateParticle, particles), particles, range(1000))
    |> List.mapi((index, {pos}) => (index, distanceFromOrigin(pos)))
    |> List.fold_left((best, curr) => snd(curr) < snd(best) ? curr : best, ((-1), max_int));
  Js.log(snd(closestToOrigin)); */
  /* Part 2 */
  let survives = (particles, (i, p1)) =>
    ! List.exists(((j, p2)) => j != i && collides(p1, p2), particles);
  let rec battleRoyale = (particlesLeft, currTime, maxTime) =>
    if (currTime > maxTime) {
      particlesLeft
    } else {
      let particles = List.mapi((i, p) => (i, updateParticle(p)), particlesLeft);
      let survivors = List.filter(survives(particles), particles);
      battleRoyale(List.map(snd, survivors), currTime + 1, maxTime)
    };
  battleRoyale(particles, 0, 100) |> List.length |> Js.log
};
