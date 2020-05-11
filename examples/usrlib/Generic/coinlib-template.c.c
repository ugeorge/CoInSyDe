module: template.c
description: "Holds standarc C templates"
literate:
  prefix: "// "
  offset: 4
---

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
