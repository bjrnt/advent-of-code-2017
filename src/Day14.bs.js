// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var $$Set              = require("bs-platform/lib/js/set.js");
var List               = require("bs-platform/lib/js/list.js");
var $$Array            = require("bs-platform/lib/js/array.js");
var Curry              = require("bs-platform/lib/js/curry.js");
var $$String           = require("bs-platform/lib/js/string.js");
var Caml_obj           = require("bs-platform/lib/js/caml_obj.js");
var Caml_array         = require("bs-platform/lib/js/caml_array.js");
var Pervasives         = require("bs-platform/lib/js/pervasives.js");
var Day10$AdventOfCode = require("./Day10.bs.js");
var Utils$AdventOfCode = require("./Utils.bs.js");

var compare = Caml_obj.caml_compare;

var PSet = $$Set.Make(/* module */[/* compare */compare]);

function formatRow(input, row) {
  return input + ("-" + Pervasives.string_of_int(row));
}

var hexToBin = (
  function(s) {
    let result = parseInt(String.fromCharCode(s),16).toString(2);
    while(result.length < 4) {
      result = "0" + result;
    }
    return result;
  }
);

function countOnes(str) {
  return List.length(List.filter((function (param) {
                      return Caml_obj.caml_equal(/* "1" */49, param);
                    }))(Utils$AdventOfCode.charsOfString(str)));
}

function exploreFrom(start, matrix) {
  var dim = matrix.length;
  var isValidNeighbor = function (param) {
    var y = param[1];
    var x = param[0];
    if (x >= 0 && y >= 0 && y < dim && x < dim) {
      return +(Caml_array.caml_array_get(Caml_array.caml_array_get(matrix, x), y) === /* "1" */49);
    } else {
      return /* false */0;
    }
  };
  var getNeighbors = function (param) {
    var y = param[1];
    var x = param[0];
    return List.filter(isValidNeighbor)(/* :: */[
                /* tuple */[
                  x + 1 | 0,
                  y
                ],
                /* :: */[
                  /* tuple */[
                    x - 1 | 0,
                    y
                  ],
                  /* :: */[
                    /* tuple */[
                      x,
                      y + 1 | 0
                    ],
                    /* :: */[
                      /* tuple */[
                        x,
                        y - 1 | 0
                      ],
                      /* [] */0
                    ]
                  ]
                ]
              ]);
  };
  var _visited = PSet[/* empty */0];
  var _queue = /* :: */[
    start,
    /* [] */0
  ];
  while(true) {
    var queue = _queue;
    var visited = _visited;
    if (queue) {
      var ps = queue[1];
      var p = queue[0];
      if (Curry._2(PSet[/* mem */2], p, visited)) {
        if (ps) {
          _queue = ps;
          continue ;
          
        } else if (Curry._2(PSet[/* mem */2], p, visited)) {
          _queue = ps;
          continue ;
          
        } else {
          return Curry._2(PSet[/* add */3], p, visited);
        }
      } else {
        _queue = Pervasives.$at(ps, getNeighbors(p));
        _visited = Curry._2(PSet[/* add */3], p, visited);
        continue ;
        
      }
    } else {
      return visited;
    }
  };
}

var bitStrings = List.map((function (row) {
        return $$String.concat("", List.map(hexToBin, Utils$AdventOfCode.charsOfString(Day10$AdventOfCode.hash(formatRow("nbysizxe", row)))));
      }), Utils$AdventOfCode.range(127));

var onesPositions = [PSet[/* empty */0]];

var matrix = $$Array.make_matrix(128, 128, /* "0" */48);

List.iteri((function (x, row) {
        return List.iteri((function (y, bit) {
                      if (bit === /* "1" */49) {
                        Caml_array.caml_array_set(Caml_array.caml_array_get(matrix, x), y, bit);
                        onesPositions[0] = Curry._2(PSet[/* add */3], /* tuple */[
                              x,
                              y
                            ], onesPositions[0]);
                        return /* () */0;
                      } else {
                        return 0;
                      }
                    }), Utils$AdventOfCode.charsOfString(row));
      }), bitStrings);

function findGroups(_groups, _visited) {
  while(true) {
    var visited = _visited;
    var groups = _groups;
    if (Curry._2(PSet[/* equal */10], visited, onesPositions[0])) {
      return groups;
    } else {
      var unvisitedPosition = Curry._1(PSet[/* choose */22], Curry._2(PSet[/* diff */8], onesPositions[0], visited));
      var group = exploreFrom(unvisitedPosition, matrix);
      _visited = Curry._2(PSet[/* union */6], group, visited);
      _groups = /* :: */[
        group,
        groups
      ];
      continue ;
      
    }
  };
}

var groups = findGroups(/* [] */0, PSet[/* empty */0]);

console.log(List.length(groups));

var hash = Day10$AdventOfCode.hash;

exports.PSet        = PSet;
exports.hash        = hash;
exports.formatRow   = formatRow;
exports.hexToBin    = hexToBin;
exports.countOnes   = countOnes;
exports.exploreFrom = exploreFrom;
/* PSet Not a pure module */