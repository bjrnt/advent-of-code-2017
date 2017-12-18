open Utils;

type register = char;

type value = int;

type registerOrValue =
  | Register(register)
  | Value(value);

type instruction =
  | Snd(registerOrValue)
  | Set(register, registerOrValue)
  | Add(register, registerOrValue)
  | Mul(register, registerOrValue)
  | Mod(register, registerOrValue)
  | Rcv(register)
  | Jgz(register, registerOrValue);

let parseRegister = (str) => str.[0];

let parseRegisterOrValue = (str) =>
  try (Value(int_of_string(str))) {
  | Failure(_) => Register(str.[0])
  };

let parseInstruction = (str) => {
  let [ins, ...params] = splitString(" ", str);
  switch ins {
  | "snd" => Snd(parseRegisterOrValue(List.nth(params, 0)))
  | "set" => Set(parseRegister(List.nth(params, 0)), parseRegisterOrValue(List.nth(params, 1)))
  | "add" => Add(parseRegister(List.nth(params, 0)), parseRegisterOrValue(List.nth(params, 1)))
  | "mul" => Mul(parseRegister(List.nth(params, 0)), parseRegisterOrValue(List.nth(params, 1)))
  | "mod" => Mod(parseRegister(List.nth(params, 0)), parseRegisterOrValue(List.nth(params, 1)))
  | "jgz" => Jgz(parseRegister(List.nth(params, 0)), parseRegisterOrValue(List.nth(params, 1)))
  | "rcv" => Rcv(parseRegister(List.nth(params, 0)))
  | _ => failwith("Unrecognized instruction: " ++ ins)
  }
};

let parseProgram = (input: string) : array(instruction) =>
  linesOfString(input) |> List.map(parseInstruction) |> Array.of_list;

let _ = {
  /* Part 1 */
  let input = loadInput("day18");
  let prog = parseProgram(input);
  let progLen = Array.length(prog);
  let mem: Hashtbl.t(char, int64) = Hashtbl.create(15);
  let readReg = (r) =>
    try (Hashtbl.find(mem, r)) {
    | Not_found => Int64.zero
    };
  let setReg = Hashtbl.replace(mem);
  let resolveRegOrVal = (regOrVal) =>
    switch regOrVal {
    | Value(v) => Int64.of_int(v)
    | Register(r) => readReg(r)
    };
  let rec runProg = (insPtr, lastSound, recoveredFrequencies) =>
    if (insPtr < 0 || insPtr >= progLen) {
      Js.log("Done!")
    } else {
      switch prog[insPtr] {
      | Snd(r) => runProg(insPtr + 1, resolveRegOrVal(r), recoveredFrequencies)
      | Set(r, rv) =>
        setReg(r, resolveRegOrVal(rv));
        runProg(insPtr + 1, lastSound, recoveredFrequencies)
      | Add(r, rv) =>
        setReg(r, Int64.add(readReg(r), resolveRegOrVal(rv)));
        runProg(insPtr + 1, lastSound, recoveredFrequencies)
      | Mul(r, rv) =>
        setReg(r, Int64.mul(readReg(r), resolveRegOrVal(rv)));
        runProg(insPtr + 1, lastSound, recoveredFrequencies)
      | Mod(r, rv) =>
        setReg(r, mod64(readReg(r), resolveRegOrVal(rv)));
        runProg(insPtr + 1, lastSound, recoveredFrequencies)
      | Rcv(r) =>
        if (readReg(r) > Int64.zero) {
          setReg(r, lastSound);
          Js.log(lastSound);
          runProg(insPtr + 1, lastSound, [lastSound, ...recoveredFrequencies])
        } else {
          runProg(insPtr + 1, lastSound, recoveredFrequencies)
        }
      | Jgz(r, rv) =>
        if (readReg(r) > Int64.zero) {
          runProg(insPtr + Int64.to_int(resolveRegOrVal(rv)), lastSound, recoveredFrequencies)
        } else {
          runProg(insPtr + 1, lastSound, recoveredFrequencies)
        }
      }
    };
  runProg(0, Int64.zero, [])
};
