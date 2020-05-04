// Format : yaml
//template:
//- name: ucosii_main
//  requirement:
//  - include: "includes.h"
//  - include: "system.h"
//  code: |
INT8U err;
    
err = OSTaskCreateExt({{Task}},
		      NULL,
		      {{TaskStack.name}} + {{int(TaskStack.type.arrSize) - 1}},
		      5,
		      5,
		      {{TaskStack.name}},
		      {{ TaskStack.type.arrSize }},
		      (void *) 0,
		      OS_TASK_OPT_STK_CHK);
OSStart();
return 0;
