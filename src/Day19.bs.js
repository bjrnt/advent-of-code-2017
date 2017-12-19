// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List               = require("bs-platform/lib/js/list.js");
var $$Array            = require("bs-platform/lib/js/array.js");
var Curry              = require("bs-platform/lib/js/curry.js");
var Caml_array         = require("bs-platform/lib/js/caml_array.js");
var Utils$AdventOfCode = require("./Utils.bs.js");

function findEntryPoint(pipes) {
  return /* record */[
          /* pos : record */[
            /* r */0,
            /* c */Utils$AdventOfCode.indexOf(/* "|" */124, $$Array.to_list(Caml_array.caml_array_get(pipes, 0)))
          ],
          /* dir : Down */1
        ];
}

function isCheckpoint(c) {
  if (c >= /* "A" */65) {
    return +(c <= /* "Z" */90);
  } else {
    return /* false */0;
  }
}

function withinBounds(pipes, param) {
  var match = param[/* pos */0];
  var c = match[/* c */1];
  var r = match[/* r */0];
  if (r > 0 && r < (pipes.length - 1 | 0) && c > 0) {
    return +(c < (Caml_array.caml_array_get(pipes, 0).length - 1 | 0));
  } else {
    return /* false */0;
  }
}

function turn(pipes, param) {
  var match = param[/* pos */0];
  var c = match[/* c */1];
  var r = match[/* r */0];
  var matchesDir = function (ch, dir) {
    var match = isCheckpoint(ch);
    if (match !== 0) {
      return /* true */1;
    } else if (dir >= 2) {
      return +(ch === /* "-" */45);
    } else {
      return +(ch === /* "|" */124);
    }
  };
  var turnValidDir = function (posA, posB) {
    var match = posA[/* pos */0];
    var match$1 = withinBounds(pipes, posA) && matchesDir(Caml_array.caml_array_get(Caml_array.caml_array_get(pipes, match[/* r */0]), match[/* c */1]), posA[/* dir */1]);
    if (match$1 !== 0) {
      return posA;
    } else {
      return posB;
    }
  };
  if (param[/* dir */1] >= 2) {
    return turnValidDir(/* record */[
                /* pos : record */[
                  /* r */r + 1 | 0,
                  /* c */c
                ],
                /* dir : Down */1
              ], /* record */[
                /* pos : record */[
                  /* r */r - 1 | 0,
                  /* c */c
                ],
                /* dir : Up */0
              ]);
  } else {
    return turnValidDir(/* record */[
                /* pos : record */[
                  /* r */r,
                  /* c */c - 1 | 0
                ],
                /* dir : Left */2
              ], /* record */[
                /* pos : record */[
                  /* r */r,
                  /* c */c + 1 | 0
                ],
                /* dir : Right */3
              ]);
  }
}

function takeStep(param) {
  var dir = param[/* dir */1];
  var match = param[/* pos */0];
  var c = match[/* c */1];
  var r = match[/* r */0];
  var tmp;
  switch (dir) {
    case 0 : 
        tmp = /* record */[
          /* r */r - 1 | 0,
          /* c */c
        ];
        break;
    case 1 : 
        tmp = /* record */[
          /* r */r + 1 | 0,
          /* c */c
        ];
        break;
    case 2 : 
        tmp = /* record */[
          /* r */r,
          /* c */c - 1 | 0
        ];
        break;
    case 3 : 
        tmp = /* record */[
          /* r */r,
          /* c */c + 1 | 0
        ];
        break;
    
  }
  return /* record */[
          /* pos */tmp,
          /* dir */dir
        ];
}

function travel(pipes, _vec, _checkpoints, _stepsTaken) {
  while(true) {
    var stepsTaken = _stepsTaken;
    var checkpoints = _checkpoints;
    var vec = _vec;
    var match = vec[/* pos */0];
    var c = Caml_array.caml_array_get(Caml_array.caml_array_get(pipes, match[/* r */0]), match[/* c */1]);
    if (c !== 32) {
      if (c !== 43) {
        if (isCheckpoint(c)) {
          _stepsTaken = stepsTaken + 1 | 0;
          _checkpoints = /* :: */[
            c,
            checkpoints
          ];
          _vec = takeStep(vec);
          continue ;
          
        } else {
          _stepsTaken = stepsTaken + 1 | 0;
          _vec = takeStep(vec);
          continue ;
          
        }
      } else {
        _stepsTaken = stepsTaken + 1 | 0;
        _vec = turn(pipes, vec);
        continue ;
        
      }
    } else {
      return /* tuple */[
              checkpoints,
              stepsTaken - 1 | 0
            ];
    }
  };
}

var input = Curry._1(Utils$AdventOfCode.loadInput, "day19");

var pipes = $$Array.map((function (line) {
        return $$Array.of_list(Utils$AdventOfCode.charsOfString(line));
      }), $$Array.of_list(Utils$AdventOfCode.linesOfString(input)));

var entryPoint = findEntryPoint(pipes);

var match = travel(pipes, entryPoint, /* [] */0, 1);

console.log("Checkpoints:", Utils$AdventOfCode.stringOfChars(List.rev(match[0])));

console.log("Steps taken:", match[1]);

exports.findEntryPoint = findEntryPoint;
exports.isCheckpoint   = isCheckpoint;
exports.withinBounds   = withinBounds;
exports.turn           = turn;
exports.takeStep       = takeStep;
exports.travel         = travel;
/* input Not a pure module */
