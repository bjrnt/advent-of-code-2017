// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var $$Set                   = require("bs-platform/lib/js/set.js");
var List                    = require("bs-platform/lib/js/list.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Hashtbl                 = require("bs-platform/lib/js/hashtbl.js");
var Caml_obj                = require("bs-platform/lib/js/caml_obj.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_format             = require("bs-platform/lib/js/caml_format.js");
var Utils$AdventOfCode      = require("./Utils.bs.js");
var Inputs$AdventOfCode     = require("./Inputs.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

var compare = Caml_obj.caml_compare;

var IntSet = $$Set.Make(/* module */[/* compare */compare]);

function parseNode(str) {
  var exit = 0;
  var match = Utils$AdventOfCode.splitString(" <-> ", str);
  if (match) {
    var match$1 = match[1];
    if (match$1) {
      if (match$1[1]) {
        exit = 1;
      } else {
        return /* tuple */[
                Caml_format.caml_int_of_string(match[0]),
                List.map(Caml_format.caml_int_of_string, Utils$AdventOfCode.splitString(", ", match$1[0]))
              ];
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
            "/Users/bjorn/projects/advent-of-code/src/Day12.re",
            12,
            6
          ]
        ];
  }
  
}

function parseGraph(str) {
  return List.map(parseNode, Utils$AdventOfCode.linesOfString(str));
}

function makeGraph(def) {
  var graph = Hashtbl.create(/* None */0, List.length(def));
  List.iter((function (param) {
          return Hashtbl.add(graph, param[0], param[1]);
        }), def);
  return graph;
}

function walkGraph(start, graph) {
  var _visited = IntSet[/* empty */0];
  var _queue = /* :: */[
    start,
    /* [] */0
  ];
  while(true) {
    var queue = _queue;
    var visited = _visited;
    if (queue) {
      var xs = queue[1];
      var x = queue[0];
      if (Curry._2(IntSet[/* mem */2], x, visited)) {
        if (xs) {
          _queue = xs;
          continue ;
          
        } else if (Curry._2(IntSet[/* mem */2], x, visited)) {
          _queue = xs;
          continue ;
          
        } else {
          return Curry._2(IntSet[/* add */3], x, visited);
        }
      } else {
        _queue = Pervasives.$at(xs, Hashtbl.find(graph, x));
        _visited = Curry._2(IntSet[/* add */3], x, visited);
        continue ;
        
      }
    } else {
      return visited;
    }
  };
}

var def = List.map(parseNode, Utils$AdventOfCode.linesOfString(Inputs$AdventOfCode.day12));

var graph = makeGraph(def);

console.log(Curry._1(IntSet[/* cardinal */18], walkGraph(0, graph)));

var allNodes = Curry._1(IntSet[/* of_list */25], List.map((function (prim) {
            return prim[0];
          }), def));

function part2(_visited, _totalGroups) {
  while(true) {
    var totalGroups = _totalGroups;
    var visited = _visited;
    if (Curry._2(IntSet[/* equal */10], allNodes, visited)) {
      return totalGroups;
    } else {
      var start = Curry._1(IntSet[/* choose */22], Curry._2(IntSet[/* diff */8], allNodes, visited));
      var clique = walkGraph(start, graph);
      _totalGroups = totalGroups + 1 | 0;
      _visited = Curry._2(IntSet[/* union */6], visited, clique);
      continue ;
      
    }
  };
}

console.log(part2(IntSet[/* empty */0], 0));

exports.IntSet     = IntSet;
exports.parseNode  = parseNode;
exports.parseGraph = parseGraph;
exports.makeGraph  = makeGraph;
exports.walkGraph  = walkGraph;
/* IntSet Not a pure module */