let generate: (int, int) => int = [%bs.raw
  {|
  function(x, y) {
    return (x * y) % 2147483647;
  }
|}
];

let genA = generate(16807);

let genB = generate(48271);

let judge: (int, int) => bool = [%bs.raw
  {|function(x, y) {
    return (x & 0b1111111111111111) === (y & 0b1111111111111111) ? 1 : 0;}
|}
];

let pickyGenerator = (multipleOf, generator, prev) => {
  let rec generateMultiple = (prev) =>
    switch (generator(prev)) {
    | i when i mod multipleOf === 0 => i
    | i => generateMultiple(i)
    };
  generateMultiple(prev)
};

let pickyGenA = pickyGenerator(4, genA);

let pickyGenB = pickyGenerator(8, genB);

let countMatches = (limit, (genA, genB), (startA, startB)) => {
  let rec judgeNums = ((numA, numB), matches, i) =>
    if (i <= limit) {
      judgeNums((genA(numA), genB(numB)), matches + (judge(numA, numB) ? 1 : 0), i + 1)
    } else {
      matches
    };
  judgeNums((startA, startB), 0, 0)
};

let _ =
  /* Part 1 */
  /* Js.log(countMatches(40_000_000, (genA, genB), (722,354))); */
  /* Part 2 */
  Js.log(countMatches(5_000_000, (pickyGenA, pickyGenB), (722, 354)));
