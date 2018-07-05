(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"), require("../../addon/mode/simple"), require("../mllike/mllike"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "../../addon/mode/simple", "../mllike/mllike"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
"use strict";

CodeMirror.defineSimpleMode("big", {
    start: [
        {regex: /\b(?:float|int|atomic ctrl|ctrl|big|react)\b/,
         token: "keyword"},
        {regex: /\b(?:share|by|in)\b/, token: "atom"},
        {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i,
         token: "number"},
        {regex: /#.*/, token: "comment"},
        {regex: /\/(?:[^\\]|\\.)*?\//, token: "variable-3"},
        {regex: /[-+\/*=<>!]+/, token: "operator"},
        {regex: /begin (?:brs|pbrs|sbrs)/, token: "keyword", next: "system"},
        {regex: /\{/, next: "name"},
        {regex: /[\{\[\(]/, indent: true},
        {regex: /[\}\]\)]/, dedent: true},
        {regex: /[a-z$][\w$]*/, token: "variable"},
        {regex: /%ocaml/, token: "meta", mode: {spec: "mllike"}},
        {regex: /%\w+/, token: "meta"}
    ],
    name: [
        {regex: /\}/, next: "start"},
        {regex: /[a-z]\w*/, token: "string"}
    ],
    system: [
        {regex: /end/, token: "keyword", next: "start"},
        {regex: /\b(?:init|rules|preds)/, token: "atom"},
        {regex: /=/, token: "operator"}
    ],
    meta: {
        lineComment: "#"
    }
});

CodeMirror.defineMIME('text/x-big', {
  name: 'big',
});

});
