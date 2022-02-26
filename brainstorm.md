(+ 4 5 (- 9 10))
-> 
``` 
    compile 4

    pushq rdi
    compile 5
    popq rdi
    add

    compile (- 9 10)
    add -> store in RDI
```
