-- module: template.c
-- description: Holds standarc C templates
-- literate:
--   prefix: "// "
--   offset: 4

// template:
// - name: Generic.Composite.Infinite
//   parameter:
//   - {name: schedule, value: "!ordered list of placeholder names"}
//   code: |
while (1) {
{% for inst in schedule %}
{{ placeholder(inst) }}
{% endfor %}
}

return 0;

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
    scanf({{ _format }}, {{ arg._name }}[__io_it]);
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

// - name: Skeleton.ShiftFarm
//   comment: |
//     Shift-farm skeleton.
// 
//     Placeholders: 
//     * 'f': skeleton template with exposed size parameter. Needs to expose
//            ports 'in1 <iarg>' and 'size1 <iarg>'
//   port:
//   - name: in1
//     value: "<iarg> Auto-bound to 'in1' from placeholder 'f'"
//   - name: _it
//     value: "<var> Internal iterator"
//   - name: _range
//     value: "<var> Auto-bound to 'size1' from placeholder 'f'"
//   parameter:
//   - name: iterate_over
//     value: ["array input port IDs"]
//   code: |
{{ setdef("iterate_over","{offset: 0, range: \"{{_type._size}}\"}") }}

{% macro itrng1() %}
{% for p in iterate_over %},{{eval(p.range, port(p._callback))}}{% endfor %}
{% endmacro %}

{% macro itrange() %}min({{slice(itrng1(), start=1, length=null)}}){% endmacro %}

for ({{ _it._name }} = 0; {{ _it._name }} < {{ itrange() }} ; {{ _it._name }}++){
  {{ _range._name }} = {{ itrange() }} - {{ _it._name }};
  {{ placeholder("f") }}
}

// - name: Skeleton.FarmReduce.Init+Fused+ExpSize
//   comment: |
//     Farm-reduce skeleton with initial element, fused map-reduce kernel function
//     and programmable (exposed) size parameter.
// 
//     Placeholders:
//     * 'f': fused map-reduce kernel function
//   port:
//   - name: _it
//     value: "<var> Internal iterator"
//   - name: _acc
//     value: "Internal accumulator variable. Needs to be initialized!"
//   parameter:
//   - name: iterate_over
//     value: ["array input port IDs"]
//   code: |
{{ setdef("iterate_over","{offset: 0, range: \"{{_type._size}}\"}") }}

{% macro itrng1() %}
{% for p in iterate_over %},{{eval(p.range, port(p._callback))}}{% endfor %}
{% endmacro %}

{% macro itrange() %}min({{slice(itrng1(), start=1, length=null)}}){% endmacro %}

for ({{ _it._name }} = 0; {{ _it._name }} < {{ itrange() }} ; {{ _it._name }}++){
  {{ placeholder("f") }}
}
{{ out1._name }} = {{ _acc._name }};
