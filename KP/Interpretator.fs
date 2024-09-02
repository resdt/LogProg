type id = string
type expr =
| App of expr*expr
| Lam of id*expr
| Var of id
| Int of int
| PFunc of id
| Cond of expr*expr*expr
| Let of id*expr*expr
| LetRec of id*expr*expr
| Op of id*int*expr list
| Closure of expr*env
| RClosure of expr*env*id
and
  env = Map<id,expr>

let funof = function
| "+" -> (fun [Int(a);Int(b)] -> Int(a+b))
| "-" -> (fun [Int(a);Int(b)] -> Int(b-a))
| "*" -> (fun [Int(a);Int(b)] -> Int(a*b))
| "/" -> (fun [Int(a);Int(b)] -> Int(b/a))
| "=" -> (fun [Int(a);Int(b)] -> if a=b then Int(1) else Int(0))
| ">" -> (fun [Int(a);Int(b)] -> if b>a then Int(1) else Int(0))
| "<" -> (fun [Int(a);Int(b)] -> if b<a then Int(1) else Int(0))
| "<=" -> (fun [Int(a);Int(b)] -> if b<=a then Int(1) else Int(0))

let rec eval exp env =
  match exp with
  | Int(n) -> Int(n)
  | Var(x) -> Map.find x env
  | Lam(id,ex) -> Closure(exp,env)
  | App(ex1,ex2) -> apply (eval ex1 env) (eval ex2 env)
  | PFunc(id) -> Op(id,2,[])
  | Cond(e0,e1,e2) ->
     if eval e0 env=Int(0) then eval e2 env else eval e1 env
  | Let(id,e1,e2) ->
     let e1' = eval e1 env in eval e2 (Map.add id e1' env)
  | LetRec(id,e1,e2) ->
     let e1' = RClosure(e1,env,id) in eval e2 (Map.add id e1' env)
and apply e1 e2 =
  match e1 with
  | Closure(Lam(v,e),env) -> eval e (Map.add v e2 env)
  | RClosure(Lam(v,e),env, id) -> eval e (Map.add v e2 (Map.add id e1 env))
  | Op(id,n,args) ->
     if n=1 then (funof id)(e2::args)
     else Op(id,n-1,e2::args)

eval (App(App(PFunc("+"),Int(1)),Int(2))) Map.empty

eval (App(Lam("x",Var("x")),Int(5))) Map.empty

eval (App(Lam("x",App(App(PFunc("*"),Var("x")),Var("x"))),Int(5))) Map.empty

let P =
   LetRec("fact",
     Lam("x",
       Cond(App(App(PFunc("<="),Var("x")),Int(1)),
         Int(1),
         App(App(PFunc("*"),Var("x")),
            App(Var("fact"),App(App(PFunc("-"),Var("x")),Int(1)))))),
     App(Var("fact"),Int(5)))

eval P Map.empty

