(function (mod) {
    if (typeof exports == "object" && typeof module == "object") // CommonJS
        mod(require("../../lib/codemirror"),
            require("../../addon/mode/simple"),
            require("../mllike/mllike"));
    else if (typeof define == "function" && define.amd) // AMD
        define(["../../lib/codemirror",
            "../../addon/mode/simple",
            "../mllike/mllike"], mod);
    else // Plain browser env
        mod(CodeMirror);
})(function (CodeMirror) {
    "use strict";

    CodeMirror.defineSimpleMode("big", {
        start: [
            {
                regex: /\d[\d_]*|inf|\d[\d_]*(?:\.[\d_]*)?(?:[eE][+-]?\d[\d_]*)?/,
                token: "number"
            },
            {
                regex: /\b(?:ctrl|atomic|big|react|int|float|fun|id|merge|split)\b/,
                token: "keyword"
            },
            { regex: /\b(?:share|by|in|init|rules|preds)\b/, token: "atom" },
            { regex: /#.*/, token: "comment" },
            { regex: /(?:->|-->|-[|]->|[=\|@+\-*/^])+/, token: "operator" },
            {
                regex: /begin (?:brs|pbrs|sbrs|nbrs)/,
                token: "keyword",
                next: "system",
                indent: true
            },
            {
                regex: /action/,
                token: "keyword",
                next: "system",
                indent: true
            },
            { regex: /\{/, next: "name" },
            { regex: /%ocaml/, token: "meta", mode: { spec: "mllike" } },
            { regex: /%\w+/, token: "meta" },
            { regex: /[a-zA-Z][\w_']*/, token: "variable" }
        ],
        name: [
            { regex: /\}/, next: "start" },
            { regex: /[a-z][\w_']*/, token: "string" }
        ],
        system: [
            { regex: /end/, token: "keyword", next: "start", dedent: true },
            {
                regex: /\d[\d_]*|inf|\d[\d_]*(?:\.[\d_]*)?(?:[eE][+-]?\d[\d_]*)?/,
                token: "number"
            },
            {
                regex: /\b(?:ctrl|atomic|big|react|int|float|fun|id|merge|split)\b/,
                token: "keyword"
            },
            { regex: /\b(?:share|by|in|init|rules|preds)\b/, token: "atom" },
            { regex: /#.*/, token: "comment" },
            { regex: /(?:->|-->|-[|]->|[=\|@+\-*/^])+/, token: "operator" },
            { regex: /[a-zA-Z][\w_']*/, token: "variable" }
        ],
        meta: {
            lineComment: "#"
        }
    });

    CodeMirror.defineMIME('text/x-big', {
        name: 'big',
    });

});
