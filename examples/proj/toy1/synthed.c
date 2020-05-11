module: synthed.c
description: Synthesized c code
literate:
  prefix: "// "
  offset: 4
---

// native:
// - name: mulacc
//   port:
//   - {name: in1, kind: iarg.2, type: Int}
//   - {name: in2, kind: iarg.3, type: Int}
//   - {name: acc, kind: iarg.4, type: Int}
//   - {name: out, kind: ret, type: Int}
//   code: |
return acc + in1 * in2;
