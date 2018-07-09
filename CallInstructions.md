Running example, compiling `a(b,1)`


* `call_`
    
      ldfun a
      push R_UnboundValue
      mkprom 1
      push 1
      mkprom 2
      ; stack : [a, (prom1, _), (prom2, 1)]
      call_ [2]

      ; or, depending on what we know about callee:
      ldfun a
      push R_UnboundValue
      mkprom 1
      push 1
      ; stack : [a, (prom1, _), 1]
      call_ [2]

    promise 1:
      ldvar b
    promise 2:
      ldconst 1


* `call_implicit_`

      ldfun a
      ; stack : [a]
      call_implicit_ [1, 2]
      ; promises allocated only if ncessary
    promise 1:
      ldvar b
    promise 2:
      ldconst 1


`static_*` same as above, but function is statically know and passed as implicit argument
