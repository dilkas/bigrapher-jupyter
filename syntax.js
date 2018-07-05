CodeMirror.defineSimpleMode("big", {
    start: [
        {regex: /(?:float|int|atomic ctrl|ctrl|big|react|begin|brs|pbrs|sbrs|end)\b/,
         token: "keyword"},
        {regex: /init|rules|preds|share|by|in/, token: "atom"},
        {regex: /0x[a-f\d]+|[-+]?(?:\.\d+|\d+\.?\d*)(?:e[-+]?\d+)?/i,
         token: "number"},
        {regex: /#.*/, token: "comment"},
        {regex: /\/(?:[^\\]|\\.)*?\//, token: "variable-3"},
        {regex: /[-+\/*=<>!]+/, token: "operator"},
        {regex: /[\{\[\(]/, indent: true},
        {regex: /[\}\]\)]/, dedent: true},
        {regex: /[a-z$][\w$]*/, token: "variable"},
        {regex: "%ocaml\n", token: "meta", mode: {spec: "ocaml"}}
    ],
    meta: {
        lineComment: "#"
    }
});
