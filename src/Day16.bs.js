// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var $$Array                 = require("bs-platform/lib/js/array.js");
var Block                   = require("bs-platform/lib/js/block.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Caml_array              = require("bs-platform/lib/js/caml_array.js");
var Caml_int32              = require("bs-platform/lib/js/caml_int32.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_format             = require("bs-platform/lib/js/caml_format.js");
var Utils$AdventOfCode      = require("./Utils.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function parseMove(str) {
  var match = Utils$AdventOfCode.charsOfString(str);
  if (match) {
    var switcher = match[0] - 112 | 0;
    if (switcher > 8 || switcher < 0) {
      return Pervasives.failwith("Unrecognized move: " + str);
    } else {
      switch (switcher) {
        case 0 : 
            var match$1 = match[1];
            if (match$1) {
              var match$2 = match$1[1];
              if (match$2) {
                var match$3 = match$2[1];
                if (match$3) {
                  if (match$3[1]) {
                    return Pervasives.failwith("Unrecognized move: " + str);
                  } else {
                    return /* Partner */Block.__(2, [
                              match$1[0],
                              match$3[0]
                            ]);
                  }
                } else {
                  return Pervasives.failwith("Unrecognized move: " + str);
                }
              } else {
                return Pervasives.failwith("Unrecognized move: " + str);
              }
            } else {
              return Pervasives.failwith("Unrecognized move: " + str);
            }
            break;
        case 3 : 
            return /* Spin */Block.__(0, [Caml_format.caml_int_of_string(Utils$AdventOfCode.stringOfChars(match[1]))]);
        case 1 : 
        case 2 : 
        case 4 : 
        case 5 : 
        case 6 : 
        case 7 : 
            return Pervasives.failwith("Unrecognized move: " + str);
        case 8 : 
            var exit = 0;
            var match$4 = Utils$AdventOfCode.splitString("/", Utils$AdventOfCode.stringOfChars(match[1]));
            if (match$4) {
              var match$5 = match$4[1];
              if (match$5) {
                if (match$5[1]) {
                  exit = 1;
                } else {
                  return /* Exchange */Block.__(1, [
                            Caml_format.caml_int_of_string(match$4[0]),
                            Caml_format.caml_int_of_string(match$5[0])
                          ]);
                }
              } else {
                exit = 1;
              }
            } else {
              exit = 1;
            }
            if (exit === 1) {
              throw [
                    Caml_builtin_exceptions.match_failure,
                    [
                      "/Users/bjorn/projects/advent-of-code/src/Day16.re",
                      12,
                      8
                    ]
                  ];
            }
            break;
        
      }
    }
  } else {
    return Pervasives.failwith("Unrecognized move: " + str);
  }
}

function indexOf(array, target) {
  var pos = [-1];
  $$Array.iteri((function (i, elem) {
          if (elem === target) {
            pos[0] = i;
            return /* () */0;
          } else {
            return 0;
          }
        }), array);
  return pos[0];
}

function swap(array, x, y) {
  var tmp = Caml_array.caml_array_get(array, x);
  Caml_array.caml_array_set(array, x, Caml_array.caml_array_get(array, y));
  Caml_array.caml_array_set(array, y, tmp);
  return array;
}

function parseMoves(str) {
  return List.map(parseMove, Utils$AdventOfCode.splitString(",", str));
}

var startingPositions = $$Array.init(16, (function (i) {
        return Pervasives.char_of_int(/* "a" */97 + i | 0);
      }));

var len = startingPositions.length;

function performMove(positions, move) {
  switch (move.tag | 0) {
    case 0 : 
        var n = move[0];
        var cpy = $$Array.copy(positions);
        var len$1 = len - n | 0;
        $$Array.blit(cpy, len$1, positions, 0, n);
        $$Array.blit(cpy, 0, positions, n, len$1);
        return positions;
    case 1 : 
        return swap(positions, move[0], move[1]);
    case 2 : 
        var match_000 = indexOf(positions, move[0]);
        var match_001 = indexOf(positions, move[1]);
        return swap(positions, match_000, match_001);
    
  }
}

function dance(positions, moves) {
  return List.fold_left(performMove, positions, moves);
}

function stringOfPositions(pos) {
  return Utils$AdventOfCode.stringOfChars($$Array.to_list(pos));
}

var moves = parseMoves(Curry._1(Utils$AdventOfCode.loadInput, "day16"));

function danceDanceDance(_knownStarts, _positions, _i) {
  while(true) {
    var i = _i;
    var positions = _positions;
    var knownStarts = _knownStarts;
    var match = indexOf($$Array.of_list(knownStarts), Utils$AdventOfCode.stringOfChars($$Array.to_list(positions)));
    if (match !== -1) {
      return List.nth(List.rev(knownStarts), Caml_int32.mod_(1000000000, i));
    } else {
      var positions$1 = $$Array.copy(positions);
      _i = i + 1 | 0;
      _positions = List.fold_left(performMove, positions$1, moves);
      _knownStarts = /* :: */[
        Utils$AdventOfCode.stringOfChars($$Array.to_list(positions)),
        knownStarts
      ];
      continue ;
      
    }
  };
}

console.log(danceDanceDance(/* [] */0, startingPositions, 0));

exports.parseMove         = parseMove;
exports.indexOf           = indexOf;
exports.swap              = swap;
exports.parseMoves        = parseMoves;
exports.startingPositions = startingPositions;
exports.len               = len;
exports.performMove       = performMove;
exports.dance             = dance;
exports.stringOfPositions = stringOfPositions;
/* startingPositions Not a pure module */