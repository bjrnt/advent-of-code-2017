open Utils;

let input = Inputs.day5;

let parseProgram = (program: string) =>
  linesOfString(program) |> List.map(int_of_string) |> Array.of_list;

let runProgram = (jumpTransformer, program) => {
  let rec stepProgram = (stepsTaken, programPointer) =>
    if (programPointer < 0 || programPointer >= Array.length(program)) {
      stepsTaken
    } else {
      let jump = program[programPointer];
      program[programPointer] = jumpTransformer(jump);
      stepProgram(stepsTaken + 1, programPointer + jump)
    };
  stepProgram(0, 0)
};

/* Part 1 */
/* Js.log(parseProgram(input) |> runProgram(jump => jump + 1)); */
/* Part 2 */
Js.log(parseProgram(input) |> runProgram((jump) => jump + (jump >= 3 ? (-1) : 1)));
