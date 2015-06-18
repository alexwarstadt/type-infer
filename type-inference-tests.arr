import shared-gdrive("type-inference-definitions.arr", "0BwTSVb9gEh9-MHlNNkJOZUVOTHc") as C
import type-infer from my-gdrive("type-inference-code.arr")

is-unification-err  = C.is-unification-err
is-unbound-id-err   = C.is-unbound-id-err
is-occurs-check-err = C.is-occurs-check-err

t-num = C.t-num
t-bool = C.t-bool
t-fun = C.t-fun
t-list = C.t-list
t-var = C.t-var




check "sweep":
  
  # static scope/shadowing
  type-infer(```
    (let ((a 1)) 
      (let ((f (lam (dummy) a))) 
        (let ((a true)) 
          (f 100))))```) is t-num
  
  type-infer("(let ((f (lam (x) a))) (let ((a 1)) (f true)))") raises-satisfies C.is-unbound-id-err

  # "generic types"
  type-infer("(lam (x) x)") is t-fun(t-var("a"), t-var("a"))

  # "nested"
  type-infer(```
    (link 
      (lam (x) 
        ((lam (f) (f x)) 
          (lam (l) (first l)))) 
      empty)```) is t-list(t-fun(t-list(t-var("a")), t-var("a")))

  # "multiple generic types"
  type-infer(```
    (link 
      (lam (x) (lam (f) (f x))) 
      empty)```) 
    is t-list(t-fun(t-var("a"), t-fun(t-fun(t-var("a"), t-var("b")), t-var("b"))))

  # "unfiication error"
  type-infer("(+ 1 true)") raises-satisfies is-unification-err

  type-infer("(lam (x) (let ((a x)) (lam (y) a)))") is t-fun(t-var("a"), t-fun(t-var("b"), t-var("a")))

  # "first of an empty list is a t-var"
  type-infer("(first empty)") is t-var("a")

  # "fun a -> b"
  type-infer("(lam (x) (first empty))") is t-fun(t-var("a"), t-var("b"))
  
end

check "basics":
  type-infer("1") is t-num
  type-infer("true") is t-bool
  type-infer("(+ 1 1)") is t-num
  type-infer("(num= 1 1)") is t-bool
  type-infer("(if true 1 2)") is t-num
  type-infer("(let ((a 1)) a)") is t-num
  type-infer("(lam (x) 1)") is t-fun(t-var("a"), t-num)
  type-infer("((lam (x) 1) true)") is t-num
  type-infer("empty") is t-list(t-var("a"))
  type-infer("(link 1 empty)") is t-list(t-num)
  type-infer("(first (link 1 empty))") is t-num
  type-infer("(rest (link 1 empty))") is t-list(t-num)
  type-infer("(is-empty (link 1 empty))") is t-bool
  type-infer("(+ 1 true)") raises-satisfies C.is-unification-err
  type-infer("(num= 1 true)") raises-satisfies C.is-unification-err
  type-infer("((lam (x) (+ x 1)) true)") raises-satisfies C.is-unification-err
  type-infer("(let ((x 1)) (if x 1 2))") raises-satisfies C.is-unification-err
  type-infer("(first 1)") raises-satisfies C.is-unification-err
  type-infer("(rest 1)") raises-satisfies C.is-unification-err
  type-infer("(is-empty 1)") raises-satisfies C.is-unification-err
  type-infer("a") raises-satisfies C.is-unbound-id-err
  type-infer("(lam (x) (x x))") raises-satisfies C.is-occurs-check-err
  type-infer("(let ((w (lam (x) (x x)))) (w w))") raises-satisfies C.is-occurs-check-err
  type-infer("(let ((l1 empty)) (link l1 l1))") raises-satisfies C.is-occurs-check-err
end

check "generic types":
  type-infer("((lam (x) 1) true)") is t-num
  type-infer("(let ((f (lam (x) x))) (let ((g (f true))) f))") is t-fun(t-bool, t-bool)
  type-infer("(let ((f (lam (x) x))) (if true f (lam (x) (+ x 1))))") is t-fun(t-num, t-num)
  type-infer("(let ((l1 empty)) (link l1 empty))") is t-list(t-list(t-var("a")))
  type-infer("(let ((l1 empty)) (lam (x) (link x l1)))") is t-fun(t-var("a"), t-list(t-var("a")))
  type-infer("(let ((l1 empty)) (let ((l2 (link 1 l1))) (lam (x) (link x l1))))") is t-fun(t-num, t-list(t-num))
  type-infer("(let ((f (lam (x) true))) (let ((a (f 1))) f))") is t-fun(t-num, t-bool)
  type-infer("(lam (x) (lam (y) (lam (x) 1)))") is t-fun(t-var("a"), t-fun(t-var("b"), t-fun(t-var("c"), t-num)))
  type-infer("(lam (x) empty)") is t-fun(t-var("a"), t-list(t-var("b")))
end

check "lists":
  type-infer("(first empty)") is t-var("a")
  type-infer("(rest empty)") is t-list(t-var("a"))
  type-infer("(link empty empty)") is t-list(t-list(t-var("a")))
  type-infer("(lam (x) (first x))") is t-fun(t-list(t-var("a")), t-var("a"))
  type-infer("(lam (x) (rest x))") is t-fun(t-list(t-var("a")), t-list(t-var("a")))
  type-infer("(link (lam (x) x) empty)") is t-list(t-fun(t-var("a"), t-var("a")))
  type-infer("(link (lam (x) 1) (link (lam (x) x) empty))") is t-list(t-fun(t-num, t-num))
  type-infer("(let ((f (lam (x) empty))) (let ((a (link f (f 1)))) f))") raises-satisfies C.is-occurs-check-err
  type-infer("(let ((f (lam (x) empty))) (let ((a (f f))) f))") raises-satisfies C.is-occurs-check-err
end

check "conditional":
  type-infer("(if true 1 2)") is t-num
  type-infer("(if true 1 2)") is t-num
  type-infer("(let ((f (if true 1 2))) (+ f f))") is t-num
  type-infer("(if true (first empty) false)") is t-bool
  type-infer("(+ 1 (if true (first empty) false))") raises-satisfies C.is-unification-err
  type-infer("(if true 1 true)") raises-satisfies C.is-unification-err
end