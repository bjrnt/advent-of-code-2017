// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var Hashtbl                 = require("bs-platform/lib/js/hashtbl.js");
var Caml_int32              = require("bs-platform/lib/js/caml_int32.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_format             = require("bs-platform/lib/js/caml_format.js");
var Utils$AdventOfCode      = require("./Utils.bs.js");
var Inputs$AdventOfCode     = require("./Inputs.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function conditionOfString(str) {
  switch (str) {
    case "!=" : 
        return /* NE */5;
    case "<" : 
        return /* LT */4;
    case "<=" : 
        return /* LE */1;
    case "==" : 
        return /* EQ */0;
    case ">" : 
        return /* GT */3;
    case ">=" : 
        return /* GE */2;
    default:
      return Pervasives.failwith("Unrecognized condition " + str);
  }
}

function operationOfString(str) {
  switch (str) {
    case "dec" : 
        return /* DEC */0;
    case "inc" : 
        return /* INC */1;
    default:
      return Pervasives.failwith("Unrecognized operation " + str);
  }
}

function parseConditional(str) {
  var match = Utils$AdventOfCode.splitString(" ", str);
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      var match$2 = match$1[1];
      if (match$2) {
        if (match$2[1]) {
          return Pervasives.failwith("Could not extract conditional from " + str);
        } else {
          return /* tuple */[
                  match[0],
                  conditionOfString(match$1[0]),
                  Caml_format.caml_int_of_string(match$2[0])
                ];
        }
      } else {
        return Pervasives.failwith("Could not extract conditional from " + str);
      }
    } else {
      return Pervasives.failwith("Could not extract conditional from " + str);
    }
  } else {
    return Pervasives.failwith("Could not extract conditional from " + str);
  }
}

function parseInstruction(str) {
  var match = Utils$AdventOfCode.splitString(" if ", str);
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      if (match$1[1]) {
        return Pervasives.failwith("Could not split on if in " + str);
      } else {
        var match$2 = Utils$AdventOfCode.splitString(" ", match[0]);
        if (match$2) {
          var match$3 = match$2[1];
          if (match$3) {
            var match$4 = match$3[1];
            if (match$4) {
              if (match$4[1]) {
                return Pervasives.failwith("Could not split front part from " + str);
              } else {
                return /* tuple */[
                        match$2[0],
                        operationOfString(match$3[0]),
                        Caml_format.caml_int_of_string(match$4[0]),
                        parseConditional(match$1[0])
                      ];
              }
            } else {
              return Pervasives.failwith("Could not split front part from " + str);
            }
          } else {
            return Pervasives.failwith("Could not split front part from " + str);
          }
        } else {
          return Pervasives.failwith("Could not split front part from " + str);
        }
      }
    } else {
      return Pervasives.failwith("Could not split on if in " + str);
    }
  } else {
    return Pervasives.failwith("Could not split on if in " + str);
  }
}

function parseProgram(lines) {
  return List.map(parseInstruction, Utils$AdventOfCode.linesOfString(lines));
}

var registry = Hashtbl.create(/* None */0, 100);

function setRegister(register, value) {
  return Hashtbl.replace(registry, register, value);
}

function getRegister(register) {
  try {
    return Hashtbl.find(registry, register);
  }
  catch (exn){
    if (exn === Caml_builtin_exceptions.not_found) {
      return 0;
    } else {
      throw exn;
    }
  }
}

function iterRegisters(fn) {
  return Hashtbl.iter(fn, registry);
}

function checkConditional(param) {
  var value = param[2];
  var curr = getRegister(param[0]);
  switch (param[1]) {
    case 0 : 
        return +(curr === value);
    case 1 : 
        return +(curr <= value);
    case 2 : 
        return +(curr >= value);
    case 3 : 
        return +(curr > value);
    case 4 : 
        return +(curr < value);
    case 5 : 
        return +(curr !== value);
    
  }
}

function runInstruction(param) {
  var value = param[2];
  var reg = param[0];
  if (checkConditional(param[3])) {
    var currentValue = getRegister(reg);
    var diff = param[1] !== 0 ? value : Caml_int32.imul(-1, value);
    return Hashtbl.replace(registry, reg, currentValue + diff | 0);
  } else {
    return 0;
  }
}

function runProgram(program) {
  return List.iter(runInstruction, program);
}

function getLargestRegister() {
  return Hashtbl.fold((function (_, curr, max) {
                var match = +(curr > max);
                if (match !== 0) {
                  return curr;
                } else {
                  return max;
                }
              }), registry, -100000);
}

var program = List.map(parseInstruction, Utils$AdventOfCode.linesOfString(Inputs$AdventOfCode.day8));

var largestSoFar = [-100000];

List.iter((function (instruction) {
        runInstruction(instruction);
        var currentLargest = getLargestRegister(/* () */0);
        if (currentLargest > largestSoFar[0]) {
          largestSoFar[0] = currentLargest;
          return /* () */0;
        } else {
          return 0;
        }
      }), program);

console.log(largestSoFar[0]);

var part2 = /* () */0;

exports.conditionOfString  = conditionOfString;
exports.operationOfString  = operationOfString;
exports.parseConditional   = parseConditional;
exports.parseInstruction   = parseInstruction;
exports.parseProgram       = parseProgram;
exports.registry           = registry;
exports.setRegister        = setRegister;
exports.getRegister        = getRegister;
exports.iterRegisters      = iterRegisters;
exports.checkConditional   = checkConditional;
exports.runInstruction     = runInstruction;
exports.runProgram         = runProgram;
exports.getLargestRegister = getLargestRegister;
exports.part2              = part2;
/* registry Not a pure module */
