provide *
provide-types *

import shared-gdrive("type-inference-definitions.arr", "0BwTSVb9gEh9-MHlNNkJOZUVOTHc") as C

t-num = C.t-num
t-bool = C.t-bool
t-fun = C.t-fun
t-list = C.t-list
t-var = C.t-var




# Feel free to modify any of the following types and function signatures. They
# are provided for your convenience.

data Constraint:
  | constraint(ty-var :: C.TypeVar, ty :: C.Type)
end

type ConstraintSet = List<Constraint>

fun gen-constraints(expr :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
    doc: ```Generates a type variable for each part of an expression,
    and constructs constraints among them.```
  cases (C.Expr) expr:
    | e-op(op, left, right) => op-help(op, left, right, ret, env)
    | e-un-op(op, ex) => un-help(op, ex, ret, env)
    | e-if(cond, consq, altern) => if-help(cond, consq, altern, ret, env)
    | e-let(name, ex, body) => let-help(name, ex, body, ret, env)
    | e-lam(param, body) => lam-help(param, body, ret, env)
    | e-app(func, arg) => app-help(func, arg, ret, env)
    | e-id(name) => link(constraint(ret, t-var(lookup(name, env))), empty)
    | e-num(value) => link(constraint(ret, t-num), empty)
    | e-bool(value) => link(constraint(ret, t-bool), empty)
    | e-empty() => link(constraint(ret, t-list(t-var(C.fresh-var()))), empty)
  end
end




fun op-help(op :: C.Operator, left :: C.Expr, right :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
  doc: "generate constraints for binary operator expressions"
  l-var = C.fresh-var()
  r-var = C.fresh-var()
  l-cons = gen-constraints(left, l-var, env)
  r-cons = gen-constraints(right, r-var, env)
  cases (C.Operator) op:
    | op-plus =>
      ret-con = constraint(ret, t-num)
      l-con = constraint(l-var, t-num)
      r-con = constraint(r-var, t-num)
      link(ret-con, link(l-con, link(r-con, l-cons.append(r-cons))))
    | op-num-eq =>
      ret-con = constraint(ret, t-bool)
      l-con = constraint(l-var, t-num)
      r-con = constraint(r-var, t-num)
      link(ret-con, link(l-con, link(r-con, l-cons.append(r-cons))))
    | op-link =>
      list-type = C.fresh-var()
      ret-con = constraint(ret, t-list(t-var(list-type)))
      l-con = constraint(l-var, t-var(list-type))
      r-con = constraint(r-var, t-list(t-var(list-type)))
      link(ret-con, link(l-con, link(r-con, l-cons.append(r-cons))))
  end
end



fun un-help(op :: C.UnaryOperator, expr :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
  doc: "generate constraints for unary operator expressions"
  expr-var = C.fresh-var()
  expr-cons = gen-constraints(expr, expr-var, env)
  list-type = C.fresh-var()
  expr-con = constraint(expr-var, t-list(t-var(list-type)))
  cases (C.UnaryOperator) op:
    | op-first => 
      ret-con = constraint(ret, t-var(list-type))
      link(ret-con, link(expr-con, expr-cons))
    | op-rest =>
      ret-con = constraint(ret, t-list(t-var(list-type)))
      link(ret-con, link(expr-con, expr-cons))
    | op-is-empty =>
      ret-con = constraint(ret, t-bool)
      link(ret-con, link(expr-con, expr-cons))
  end
end



fun if-help(cond :: C.Expr, consq :: C.Expr, altern :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
  doc: "generate constraints for if expressions"
  cond-var = C.fresh-var()
  consq-var = C.fresh-var()
  altern-var = C.fresh-var()
  cond-cons = gen-constraints(cond, cond-var, env)
  consq-cons = gen-constraints(consq, consq-var, env)
  altern-cons = gen-constraints(altern, altern-var, env)
  cond-con = constraint(cond-var, t-bool)
  branch-con = constraint(consq-var, t-var(altern-var))
  ret-con = constraint(ret, t-var(consq-var))
  link(ret-con, link(branch-con, link(cond-con, cond-cons.append(consq-cons.append(altern-cons)))))
end

fun let-help(name :: String, expr :: C.Expr, body :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
  doc: "generate constraints for let expressions"
  expr-var = C.fresh-var()
  body-var = C.fresh-var()
  expr-cons = gen-constraints(expr, expr-var, env)
  new-env = link(C.type-cell(name, expr-var), env)
  body-cons = gen-constraints(body, body-var, new-env)
  ret-con = constraint(ret, t-var(body-var))
  link(ret-con, expr-cons.append(body-cons))
end

fun lam-help(param :: String, body :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
  doc: "generate constraints for lambda expressions"
  body-var = C.fresh-var()
  param-var = C.fresh-var()
  new-env = link(C.type-cell(param, param-var), env)
  body-cons = gen-constraints(body, body-var, new-env)
  ret-con = constraint(ret, t-fun(t-var(param-var), t-var(body-var)))
  link(ret-con, body-cons)
end

fun app-help(func :: C.Expr, arg :: C.Expr, ret :: C.TypeVar, env :: C.TEnv) -> ConstraintSet:
  doc: "generate constraints for function application expressions"
  func-var = C.fresh-var()
  arg-var = C.fresh-var()
  func-cons = gen-constraints(func, func-var, env)
  arg-cons = gen-constraints(arg, arg-var, env)
  func-con = constraint(func-var, t-fun(t-var(arg-var), t-var(ret)))
  link(func-con, arg-cons.append(func-cons))
end

fun lookup(name :: String, env :: C.TEnv) -> C.TypeVar:
  doc: "find the binding with the given identifier name in the type environment and return the corresponding type-var"
  cases (List) env:
    | empty => raise(C.unbound-id-err(name))
    | link(hd, tl) =>
      if name == hd.id-name:
        hd.var-type
      else:
        lookup(name, tl)
      end
  end
end




fun flatten(l :: List<List>) -> List:
  doc: "turn a list of lists into a list"
  cases (List) l:
    | empty => empty
    | link(hd, tl) => hd.append(flatten(tl))
  end
end






fun compare-tys(ty1 :: C.Type, ty2 :: C.Type) -> ConstraintSet:
  doc: "check that two types are consistent, and add constraints where they are not identical but still consistent; only called when two constraints have the same ty-var"
  if C.is-t-var(ty1):
    [list: constraint(ty1.name, ty2)]
  else:
    if C.is-t-var(ty2):
      [list: constraint(ty2.name, ty1)]
    else:
      cases (C.Constructor) ty1.con:
        | c-num =>
          if C.is-c-num(ty2.con):
            empty
          else:
            raise(C.unification-err(ty1, ty2))
          end
        | c-bool =>
          if C.is-c-bool(ty2.con):
            empty
          else:
            raise(C.unification-err(ty1, ty2))
          end
        | c-fun =>
          if C.is-c-fun(ty2.con):
            arg-cons = compare-tys(ty1.fields.get(0), ty2.fields.get(0))
            ret-cons = compare-tys(ty1.fields.get(1), ty2.fields.get(1))
            arg-cons.append(ret-cons)
          else:
            raise(C.unification-err(ty1, ty2))
          end
        | c-list =>
          if C.is-c-list(ty2.con):
            compare-tys(ty1.fields.get(0), ty2.fields.get(0))
          else:
            raise(C.unification-err(ty1, ty2))
          end
      end
    end
  end
end



fun sub-or-grow(sub :: Constraint, in :: Constraint) -> ConstraintSet:
  doc: "compare types if two constraints have same ty-var, otherwise substitute"
  if sub.ty-var == in.ty-var:
    compare-tys(sub.ty, in.ty)
  else:
    [list: constraint(in.ty-var, substitute(sub, in.ty))]
  end
end

fun substitute(sub :: Constraint, in-ty :: C.Type) -> C.Type:
  doc: "substitute all instances of the constraint in the given type"
  if C.is-t-var(in-ty):
    if sub.ty-var == in-ty.name:
      sub.ty
    else:
      in-ty
    end
  else:
    cases (C.Constructor) in-ty.con:
      | c-fun => C.t-con(C.c-fun, [list: substitute(sub, in-ty.fields.get(0)), substitute(sub, in-ty.fields.get(1))])
      | c-list => C.t-con(C.c-list, [list: substitute(sub, in-ty.fields.get(0))])
      | else => in-ty
    end
  end
end



fun contains(v :: C.TypeVar, ty :: C.Type) -> Boolean:
  doc: "perform the contains check, i.e. ty-var of a constraint is not contained within its corresponding type"
  if C.is-t-var(ty):
    v == ty.name
  else:
    cases (C.Constructor) ty.con:
      | c-num => false
      | c-bool => false
      | c-fun => contains(v, ty.fields.get(0)) or contains(v, ty.fields.get(1))
      | c-list => contains(v, ty.fields.get(0))
    end
  end
end

fun unify(constraints :: ConstraintSet, sub :: ConstraintSet) -> ConstraintSet:
  doc: ```Perform type unification to solve a set of constraints.```
  cases (ConstraintSet) constraints:
    | empty => sub
    | link(hd, tl) =>
      if contains(hd.ty-var, hd.ty):
        raise(C.occurs-check-err(hd.ty-var, hd.ty))
      else:
        new-cons = flatten(tl.map(sub-or-grow(hd, _)))
        new-sub = link(hd, sub.map(lam(x): constraint(x.ty-var, substitute(hd, x.ty)) end))
        unify(new-cons, new-sub)
      end
  end
end

fun type-infer(e :: String) -> C.Type:
  doc: "parses and infers the type of the expression"
  expr = C.parse(e)
  ret = C.fresh-var()
  constraints = gen-constraints(expr, ret, empty)
  sub = unify(constraints, empty)
  C.normalize-type(pretty-print(ret, sub))
end

fun pretty-print(ret :: C.TypeVar, sub :: ConstraintSet) -> C.Type:
  doc: "replace all instances of a ty-var in a type with the type contained in the substitution"
  fun helper(ty :: C.Type, shadow sub :: ConstraintSet) -> C.Type:
    if C.is-t-var(ty):
      find-in-sub(ty.name, sub)
    else:
      cases (C.Constructor) ty.con:
        | c-num => ty
        | c-bool => ty
        | c-fun => C.t-con(C.c-fun, [list: helper(ty.fields.get(0), sub), helper(ty.fields.get(1), sub)])
        | c-list => C.t-con(C.c-list, [list: helper(ty.fields.get(0), sub)])
      end 
    end
  end
  helper(find-in-sub(ret, sub), sub)
end

fun find-in-sub(name :: String, sub :: ConstraintSet) -> C.Type:
  doc: "find the constraint corresponding to the given type-var in the substitution"
  cases (List) sub:
    | empty => t-var(name)
    | link(hd, tl) => 
      if hd.ty-var == name: hd.ty else: find-in-sub(name, tl) end
  end
end



