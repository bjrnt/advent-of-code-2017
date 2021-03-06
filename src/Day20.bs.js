// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var Caml_obj                = require("bs-platform/lib/js/caml_obj.js");
var Caml_array              = require("bs-platform/lib/js/caml_array.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_format             = require("bs-platform/lib/js/caml_format.js");
var Utils$AdventOfCode      = require("./Utils.bs.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function add(v1, v2) {
  return /* record */[
          /* x */v1[/* x */0] + v2[/* x */0] | 0,
          /* y */v1[/* y */1] + v2[/* y */1] | 0,
          /* z */v1[/* z */2] + v2[/* z */2] | 0
        ];
}

function collides(param, param$1) {
  var p2 = param$1[/* pos */0];
  var p1 = param[/* pos */0];
  if (p1[/* x */0] === p2[/* x */0] && p1[/* y */1] === p2[/* y */1]) {
    return +(p1[/* z */2] === p2[/* z */2]);
  } else {
    return /* false */0;
  }
}

function parseTuple(re, str) {
  var match = str.match(re);
  if (match !== null) {
    var exit = 0;
    var match$1 = List.map(Caml_format.caml_int_of_string, Utils$AdventOfCode.splitString(",", Caml_array.caml_array_get(match, 1)));
    if (match$1) {
      var match$2 = match$1[1];
      if (match$2) {
        var match$3 = match$2[1];
        if (match$3) {
          if (match$3[1]) {
            exit = 1;
          } else {
            return /* record */[
                    /* x */match$1[0],
                    /* y */match$2[0],
                    /* z */match$3[0]
                  ];
          }
        } else {
          exit = 1;
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
              "/Users/bjorn/projects/advent-of-code/src/Day20.re",
              23,
              8
            ]
          ];
    }
    
  } else {
    return Pervasives.failwith("Could not match regex " + str);
  }
}

function parseParticle(str) {
  return /* record */[
          /* pos */parseTuple((/p=<([0-9\-,]+)>/), str),
          /* vel */parseTuple((/v=<([0-9\-,]+)>/), str),
          /* acc */parseTuple((/a=<([0-9\-,]+)>/), str)
        ];
}

function distanceFromOrigin(param) {
  return (Pervasives.abs(param[/* x */0]) + Pervasives.abs(param[/* y */1]) | 0) + Pervasives.abs(param[/* z */2]) | 0;
}

function updateParticle(param) {
  var acc = param[/* acc */2];
  var vel = add(param[/* vel */1], acc);
  var pos = add(param[/* pos */0], vel);
  return /* record */[
          /* pos */pos,
          /* vel */vel,
          /* acc */acc
        ];
}

var input = Curry._1(Utils$AdventOfCode.loadInput, "day20");

var particles = List.map(parseParticle, Utils$AdventOfCode.linesOfString(input));

function battleRoyale(_particlesLeft, _currTime, maxTime) {
  while(true) {
    var currTime = _currTime;
    var particlesLeft = _particlesLeft;
    if (currTime > maxTime) {
      return particlesLeft;
    } else {
      var particles = List.mapi((function (i, p) {
              return /* tuple */[
                      i,
                      updateParticle(p)
                    ];
            }), particlesLeft);
      var survivors = List.filter((function(particles){
            return function (param) {
              var particles$1 = particles;
              var param$1 = param;
              var p1 = param$1[1];
              var i = param$1[0];
              return 1 - List.exists((function (param) {
                            if (Caml_obj.caml_notequal(param[0], i)) {
                              return collides(p1, param[1]);
                            } else {
                              return /* false */0;
                            }
                          }), particles$1);
            }
            }(particles)))(particles);
      _currTime = currTime + 1 | 0;
      _particlesLeft = List.map((function (prim) {
              return prim[1];
            }), survivors);
      continue ;
      
    }
  };
}

console.log(List.length(battleRoyale(particles, 0, 100)));

exports.add                = add;
exports.collides           = collides;
exports.parseTuple         = parseTuple;
exports.parseParticle      = parseParticle;
exports.distanceFromOrigin = distanceFromOrigin;
exports.updateParticle     = updateParticle;
/* input Not a pure module */
