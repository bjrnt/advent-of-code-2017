// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var List                    = require("bs-platform/lib/js/list.js");
var $$Array                 = require("bs-platform/lib/js/array.js");
var Curry                   = require("bs-platform/lib/js/curry.js");
var $$String                = require("bs-platform/lib/js/string.js");
var Caml_int64              = require("bs-platform/lib/js/caml_int64.js");
var Pervasives              = require("bs-platform/lib/js/pervasives.js");
var Caml_string             = require("bs-platform/lib/js/caml_string.js");
var Caml_builtin_exceptions = require("bs-platform/lib/js/caml_builtin_exceptions.js");

function splitString(sep, str) {
  return $$Array.to_list(str.split(sep));
}

function linesOfString(param) {
  return $$Array.to_list(param.split("\n"));
}

function wordsOfString(param) {
  return $$Array.to_list(param.split(" "));
}

function charsOfString(s) {
  var _i = s.length - 1 | 0;
  var _l = /* [] */0;
  while(true) {
    var l = _l;
    var i = _i;
    var match = +(i < 0);
    if (match !== 0) {
      return l;
    } else {
      _l = /* :: */[
        Caml_string.get(s, i),
        l
      ];
      _i = i - 1 | 0;
      continue ;
      
    }
  };
}

function mod64(x, y) {
  var x$1 = x;
  while(Caml_int64.gt(x$1, y)) {
    x$1 = Caml_int64.sub(x$1, y);
  };
  return x$1;
}

function stringOfChars(chars) {
  return $$String.concat("", List.map((function (param) {
                    return $$String.make(1, param);
                  }), chars));
}

function zip2(xs, ys) {
  return List.fold_left2((function (list, x, y) {
                return /* :: */[
                        /* tuple */[
                          x,
                          y
                        ],
                        list
                      ];
              }), /* [] */0, xs, ys);
}

function range(until) {
  var makeRange = function (_list, _next) {
    while(true) {
      var next = _next;
      var list = _list;
      var match = +(next > until);
      if (match !== 0) {
        return list;
      } else {
        _next = next + 1 | 0;
        _list = /* :: */[
          next,
          list
        ];
        continue ;
        
      }
    };
  };
  return List.rev(makeRange(/* [] */0, 0));
}

function isSome(param) {
  if (param) {
    return /* true */1;
  } else {
    return /* false */0;
  }
}

function expect(str, x) {
  if (x) {
    return x[0];
  } else {
    return Pervasives.failwith(str);
  }
}

function $great$great(f, g, x) {
  return Curry._1(g, Curry._1(f, x));
}

function remember(pred, reference, nextValue) {
  if (Curry._2(pred, nextValue, reference[0])) {
    reference[0] = nextValue;
  }
  return nextValue;
}

var decToBin = (
function(s) {
  let result = s.toString(2);
  while(result.length < 32) {
    result = "0" + result;
  }
  return result;
}
);

var loadInput = (
  function (filename) {
    return require('fs').readFileSync(`${__dirname}/inputs/${filename}.txt`).toString();
  }
);

function indexOf(e, xs) {
  if (xs) {
    var match = +(xs[0] === e);
    if (match !== 0) {
      return 0;
    } else {
      return 1 + indexOf(e, xs[1]) | 0;
    }
  } else {
    throw [
          Caml_builtin_exceptions.failure,
          "Couldn't find element"
        ];
  }
}

exports.splitString   = splitString;
exports.linesOfString = linesOfString;
exports.wordsOfString = wordsOfString;
exports.charsOfString = charsOfString;
exports.mod64         = mod64;
exports.stringOfChars = stringOfChars;
exports.zip2          = zip2;
exports.range         = range;
exports.isSome        = isSome;
exports.expect        = expect;
exports.$great$great  = $great$great;
exports.remember      = remember;
exports.decToBin      = decToBin;
exports.loadInput     = loadInput;
exports.indexOf       = indexOf;
/* decToBin Not a pure module */
