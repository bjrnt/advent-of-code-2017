exception InvalidDirection;

let turnLeft = (dir) =>
  switch dir {
  | (1, 0) => (0, 1)
  | (0, 1) => ((-1), 0)
  | ((-1), 0) => (0, (-1))
  | (0, (-1)) => (1, 0)
  | _ => raise(InvalidDirection)
  };

let walkUntil = (num) => {
  let sideLength = ref(1);
  let direction = ref((1, 0));
  let coordinates = ref((0, 0));
  let stepsUntilChange = ref(1);
  let sidesUntilChange = ref(2);
  let currentValue = ref(1);
  while (currentValue^ < num) {
    if (stepsUntilChange^ === 0) {
      /* Js.log("Time to turn"); */
      sidesUntilChange := sidesUntilChange^ - 1;
      if (sidesUntilChange^ === 0) {
        /* Js.log("Time to increase side length"); */
        sideLength := sideLength^ + 1;
        sidesUntilChange := 2
      };
      direction := turnLeft(direction^);
      stepsUntilChange := sideLength^
    };
    let (curr_x, curr_y) = coordinates^;
    let (dir_x, dir_y) = direction^;
    coordinates := (curr_x + dir_x, curr_y + dir_y);
    currentValue := currentValue^ + 1;
    stepsUntilChange := stepsUntilChange^ - 1
  };
  /* Js.log3(coordinates^, direction^, currentValue^); */
  let (x, y) = coordinates^;
  abs(x) + abs(y)
};

/* Js.log(walkUntil(277678)); */
/* Part 2 */
let safeFind = (tbl, loc) =>
  switch (Hashtbl.find(tbl, loc)) {
  | value => value
  | exception Not_found => 0
  };

let sumNeighbors = (tbl, (origX, origY)) => {
  let sum = ref(0);
  for (x in origX - 1 to origX + 1) {
    for (y in origY - 1 to origY + 1) {
      sum := sum^ + safeFind(tbl, (x, y))
    }
  };
  sum^
};

let computeTable = (untilFirstAbove) => {
  let tbl = Hashtbl.create(1000);
  Hashtbl.add(tbl, (0,0), 1);
  let sideLength = ref(1);
  let direction = ref((1, 0));
  let coordinates = ref((0, 0));
  let stepsUntilChange = ref(1);
  let sidesUntilChange = ref(2);
  let currentValue = ref(1);
  while (currentValue^ <= untilFirstAbove) {
    if (stepsUntilChange^ === 0) {
      /* Js.log("Time to turn"); */
      sidesUntilChange := sidesUntilChange^ - 1;
      if (sidesUntilChange^ === 0) {
        /* Js.log("Time to increase side length"); */
        sideLength := sideLength^ + 1;
        sidesUntilChange := 2
      };
      direction := turnLeft(direction^);
      stepsUntilChange := sideLength^
    };
    let (curr_x, curr_y) = coordinates^;
    let (dir_x, dir_y) = direction^;
    coordinates := (curr_x + dir_x, curr_y + dir_y);
    currentValue := sumNeighbors(tbl, coordinates^);
    Hashtbl.add(tbl, coordinates^, currentValue^);
    stepsUntilChange := stepsUntilChange^ - 1;
  };
  currentValue^;
};

Js.log(computeTable(277678));
