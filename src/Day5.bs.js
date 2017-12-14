// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                = require("bs-platform/lib/js/list.js");
var $$Array             = require("bs-platform/lib/js/array.js");
var Curry               = require("bs-platform/lib/js/curry.js");
var Caml_array          = require("bs-platform/lib/js/caml_array.js");
var Caml_format         = require("bs-platform/lib/js/caml_format.js");
var Utils$AdventOfCode  = require("./Utils.bs.js");
var Inputs$AdventOfCode = require("./Inputs.bs.js");

function parseProgram(program) {
  return $$Array.of_list(List.map(Caml_format.caml_int_of_string, Utils$AdventOfCode.linesOfString(program)));
}

function runProgram(jumpTransformer, program) {
  var _stepsTaken = 0;
  var _programPointer = 0;
  while(true) {
    var programPointer = _programPointer;
    var stepsTaken = _stepsTaken;
    if (programPointer < 0 || programPointer >= program.length) {
      return stepsTaken;
    } else {
      var jump = Caml_array.caml_array_get(program, programPointer);
      Caml_array.caml_array_set(program, programPointer, Curry._1(jumpTransformer, jump));
      _programPointer = programPointer + jump | 0;
      _stepsTaken = stepsTaken + 1 | 0;
      continue ;
      
    }
  };
}

console.log(runProgram((function (jump) {
            var match = +(jump >= 3);
            return jump + (
                    match !== 0 ? -1 : 1
                  ) | 0;
          }), parseProgram(Inputs$AdventOfCode.day5)));

var input = Inputs$AdventOfCode.day5;

exports.input        = input;
exports.parseProgram = parseProgram;
exports.runProgram   = runProgram;
/*  Not a pure module */
