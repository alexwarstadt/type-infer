# type-infer

The code is all included in type-inference-code.

Infer the type of a program by calling type-infer on it. 

The inputted string is parsed into abstract syntax by an outside parser.

The most general type is returned.



Tests are included in type-inference-tests. The test suite is a comprehensive specification

of the desired functionality. The `sweep' at the beginning includes interesting cases,

while the following tests are more comprehensive.


Type inference works in three parts:



.

.

.

.

.


------------------ 1. Constraint generation --------------------

This stage is rooted at gen-constraints. 

This function recursively descends through the abstract syntax and along with its helpers 

it generates a set of constraints. Each subexpression is assigned a unique identifier which

is constrained either to a concrete type (e.g. num, bool-->num, list(num-->num)), or a type

variable associating it with the type of another subexpression.

Constraint set example {{ (expr1 := num), (expr2 := bool-->type-of(expr1)) }}





.

.

.

.

.


----------------------- 2. Unification -------------------------

The set of constraints produced by gen-constraints is iterated over. For each constraint,

all instances of the left-hand-side as type variables are substituted with the right-hand-side.

E.g. {{ (expr1 := num), (expr2 := bool-->type-of(expr1)) }} ==> {{ (expr2 := bool-->num) }}


New constraints are added as needed, 

e.g.: {{ (expr1 := type-of(expr2)), (expr1 := list(num)) }} ==> {{ (expr2 := list(num)), (expr1 := list(num)) }}


An error is returned if any type mismatches are encountered,

e.g.: {{ (expr1 := num), (expr1 := bool) }} ==> unification-error


Unification produces a substitution, which is the constraint set that results from performing all

possible substitutions.



.

.

.

.

.



------------------------ 3. Printing ---------------------------

pretty-print takes the substitution and the variable for the return type of the entire program. It

performs all possible substitutions in the return type using the information in the substitution. The

resulting type is normalized, i.e. any remaining type-variables are renamed starting at 0.




.

.

.

.

.




------------------- Grammar for target language --------------------

<expr> ::= 

         | <num>

         | <id>
         
         | true | false
         
         | (+ <expr> <expr>)
         
         | (num= <expr> <expr>)
         
         | (link <expr> <expr>)
         
         | (if <expr> <expr> <expr>)
         
         | (lam (<id>) <expr>)
         
         | (let ((<id> <expr>)) <expr>)
         
         | (<expr> <expr>)
         
         | (first <expr>)
         
         | (rest <expr>)
         
         | (is-empty <expr>)
         
         | empty
