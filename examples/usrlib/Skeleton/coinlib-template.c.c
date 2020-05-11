module: Skeleton
target: c
what:   template
description: "Holds standard C templates"
literate:
  prefix: "// "
  offset: 4
---

// template:
// - name: Skeleton.ShiftFarm
//   comment: |
//     Shift-farm skeleton.
// 
//     Placeholders: 
//     * 'f': skeleton template with exposed size parameter. Needs to expose
//            ports 'in1 <iarg>' and 'size1 <iarg>'
//   port:
//   - name: _it
//     value: "<var> Internal iterator"
//   - name: _range
//     value: "<var> Auto-bound to 'size1' from placeholder 'f'"
//   parameter:
//   - name: iterate_over
//     value: "list of port IDs over which '_it' will iterate"
//   requirement:
//   - include: cutils.h
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
//   requirement:
//   - include: cutils.h
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
