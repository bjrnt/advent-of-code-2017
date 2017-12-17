type node = {
  value: int,
  next: ref(node)
};

let rec step = (node, steps) => steps === 0 ? node : step(node.next^, steps - 1);

let insertAfter = (node, value) => {
  let next = node.next^;
  node.next := {value, next: ref(next)};
  node.next^
};

let rec spinlock = (node: node, stepsPerInsert: int, nextValue: int, insertUntil: int) =>
  if (nextValue > insertUntil) {
    node.next^.value
  } else {
    let currentNode = step(node, stepsPerInsert);
    spinlock(insertAfter(currentNode, nextValue), stepsPerInsert, nextValue + 1, insertUntil)
  };

let _ = {
  let stepsPerInsert = 337;
  let rec startNode = {value: 0, next: ref(startNode)};
  /* Part 1 */
  spinlock(startNode, stepsPerInsert, 1, 2017) |> Js.log;
  /* Part 2 */
  let limit = 50_000_000;
  let rec part2 = (afterZero, position, next) =>
    if (next > limit) {
      afterZero
    } else {
      let nextPosition = (position + stepsPerInsert) mod next;
      part2(nextPosition === 0 ? next : afterZero, nextPosition + 1, next + 1)
    };
  Js.log(part2(0, 0, 1));
};
