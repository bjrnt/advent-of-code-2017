open Utils;

type register = string;

type registers = Hashtbl.t(register, int);

type condition =
  | EQ
  | LE
  | GE
  | GT
  | LT
  | NE;

type operation =
  | DEC
  | INC;

type conditional = (register, condition, int);

type instruction = (register, operation, int, conditional);

let conditionOfString = (str) =>
  switch str {
  | "==" => EQ
  | "<=" => LE
  | "!=" => NE
  | "<" => LT
  | ">" => GT
  | ">=" => GE
  | _ => failwith("Unrecognized condition " ++ str)
  };

let operationOfString = (str) =>
  switch str {
  | "dec" => DEC
  | "inc" => INC
  | _ => failwith("Unrecognized operation " ++ str)
  };

let parseConditional = (str: string) : conditional =>
  switch (splitString(" ", str)) {
  | [register, conditionStr, value] => (
      register,
      conditionOfString(conditionStr),
      int_of_string(value)
    )
  | _ => failwith("Could not extract conditional from " ++ str)
  };

let parseInstruction = (str: string) : instruction =>
  switch (splitString(" if ", str)) {
  | [frontPart, conditionalStr] =>
    switch (splitString(" ", frontPart)) {
    | [register, operationStr, value] => (
        register,
        operationOfString(operationStr),
        int_of_string(value),
        parseConditional(conditionalStr)
      )
    | _ => failwith("Could not split front part from " ++ str)
    }
  | _ => failwith("Could not split on if in " ++ str)
  };

let parseProgram = (lines: string) : list(instruction) =>
  linesOfString(lines) |> List.map(parseInstruction);

let registry = Hashtbl.create(100);

let setRegister = (register: register, value: int) : unit =>
  Hashtbl.replace(registry, register, value);

let getRegister = (register: register) : int =>
  try (Hashtbl.find(registry, register)) {
  | Not_found => 0
  };

let iterRegisters = (fn: (register, int) => unit) => Hashtbl.iter(fn, registry);

let checkConditional = ((reg, cond, value): conditional) => {
  let curr = getRegister(reg);
  switch cond {
  | EQ => curr == value
  | NE => curr != value
  | LT => curr < value
  | GT => curr > value
  | GE => curr >= value
  | LE => curr <= value
  }
};

let runInstruction = ((reg, op, value, cond): instruction) : unit =>
  if (checkConditional(cond)) {
    let currentValue = getRegister(reg);
    let diff =
      switch op {
      | INC => value
      | DEC => (-1) * value
      };
    setRegister(reg, currentValue + diff)
  };

let runProgram = (program: list(instruction)) : unit => List.iter(runInstruction, program);

let getLargestRegister = () =>
  Hashtbl.fold((_, curr, max) => curr > max ? curr : max, registry, (-100000));

/* let part1 = {
     let program = parseProgram(Inputs.day8);
     runProgram(program);
     Js.log(getLargestRegister())
   }; */
let part2 = {
  let program = parseProgram(Inputs.day8);
  let largestSoFar = ref((-100000));
  List.iter(
    (instruction) => {
      runInstruction(instruction);
      let currentLargest = getLargestRegister();
      if (currentLargest > largestSoFar^) {
        largestSoFar := currentLargest
      }
    },
    program
  );
  Js.log(largestSoFar^)
};
