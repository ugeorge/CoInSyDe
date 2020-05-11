module: template.c
description: "Holds standarc C templates"
literate:
  prefix: "// "
  offset: 4
---

// template:
// - name: IO.Std.ScanfArray
//   port:
//   - {name: arg, value: "<iarg> array type"} 
//   parameter:
//   - {name: format, value: "!format for scanf"}
//   requirement:
//   - {include: stdio.h}
//   code: |
{int __io_it;
  for (__io_it = 0; __io_it < {{arg._type._size}}; __io_it++){
    scanf({{ _format }}, &{{ arg._name }}[__io_it]);
  }
}

// - name: IO.Std.PrintfArray
//   port:
//   - {name: arg, value: "<iarg> array type"}
//   parameter:
//   - {name: format, value: "!format for printf"}
//   requirement:
//   - {include: stdio.h}
//   code: |
{int __io_it;
  for (__io_it = 0; __io_it < {{arg._type._size}}; __io_it++){
    printf({{ _format }}, {{ arg._name }}[__io_it]);
  }
}
