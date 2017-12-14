// Generated by BUCKLESCRIPT VERSION 2.1.0, PLEASE EDIT WITH CARE
'use strict';

var $$Set               = require("bs-platform/lib/js/set.js");
var Char                = require("bs-platform/lib/js/char.js");
var List                = require("bs-platform/lib/js/list.js");
var Curry               = require("bs-platform/lib/js/curry.js");
var $$String            = require("bs-platform/lib/js/string.js");
var Utils$AdventOfCode  = require("./Utils.bs.js");
var Inputs$AdventOfCode = require("./Inputs.bs.js");

var StringSet = $$Set.Make([$$String.compare]);

function isValidPassword(str) {
  var words = Utils$AdventOfCode.wordsOfString(str);
  return +(List.length(Curry._1(StringSet[/* elements */19], Curry._1(StringSet[/* of_list */25], words))) === List.length(words));
}

function countValidPasswords(validator, lines) {
  return List.fold_left((function (count, line) {
                var match = Curry._1(validator, line);
                return count + (
                        match !== 0 ? 1 : 0
                      ) | 0;
              }), 0, Utils$AdventOfCode.linesOfString(lines));
}

function isValidAnagramPassword(str) {
  var charSortedWords = List.map((function (word) {
          return Utils$AdventOfCode.charsToString(List.sort(Char.compare, Utils$AdventOfCode.charsOfString(word)));
        }), Utils$AdventOfCode.wordsOfString(str));
  return +(List.length(Curry._1(StringSet[/* elements */19], Curry._1(StringSet[/* of_list */25], charSortedWords))) === List.length(charSortedWords));
}

var input = Inputs$AdventOfCode.day4;

exports.input                  = input;
exports.StringSet              = StringSet;
exports.isValidPassword        = isValidPassword;
exports.countValidPasswords    = countValidPasswords;
exports.isValidAnagramPassword = isValidAnagramPassword;
/* StringSet Not a pure module */
