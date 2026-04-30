module rec Fw.Ast

open FSharp.Compiler.Symbols

type Metadata = class end

type CppTy =
  | Int
  | Bool
  | Void
  | Named of string
  | Auto
  | Gen of template: string * args: CppTy list

type ArgSig = (string * CppTy) list

type FunctionSignature = { args: ArgSig; rt: CppTy }

type CppClass = {
  name: string
  inherits: string list
  decls: CppDecl list
}

type CppStruct = CppClass

type CppDecl =
  | Namespace of name: string * decls: CppDecl list
  | Template of args: string list * decl: CppDecl
  | Class of CppClass
  | Function of
    name: string *
    signature: FunctionSignature *
    body: CppStmt list option
  | Constructor of
    tyName: string *
    args: ArgSig *
    body: CppStmt list option
  | Struct of CppStruct
  | Comment of string
  | Variable of
    name: string *
    ty: CppTy *
    value: CppExpr option
  /// Synthetic: Helper so functions can return multiple declarations
  | Sequence of CppDecl list

type CppExpr =
  | Var of string
  | Const of obj * FSharpType
  | Call of callee: CppExpr * args: CppExpr list
  | CallGen of
    callee: CppExpr *
    genArgs: CppExpr list *
    args: CppExpr list
  | Lambda of
    args: string list *
    body: CppStmt list *
    captures: string list
  | ExprComment of string
  | GetField of src: CppExpr * field: string
  | DerefGetField of src: CppExpr * field: string

  static let sourceMappings =
    System.Runtime.CompilerServices.ConditionalWeakTable<
      CppExpr,
      Metadata
     >()

  member this.Metadata =
    sourceMappings.TryGetValue(this)
    |> function
      | true, metadata -> Some metadata
      | false, _ -> None

and CppStmt =
  | Let of name: string * value: CppExpr
  | Exp of CppExpr
  | SComment of string
  | Return of CppExpr
  | Assign of dest: CppExpr * value: CppExpr
  | ForLoop of
    init: CppStmt *
    cond: CppExpr *
    post: CppStmt *
    body: CppStmt list
  | WhileLoop of cond: CppExpr * body: CppStmt list
  | IfThenElse of
    cond: CppExpr *
    wt: CppStmt list *
    wf: CppStmt list
  | Scope of CppStmt list
  | SVariable of
    name: string *
    ty: CppTy *
    value: CppExpr option
  | TryCatch of
    tryBlock: CppStmt list *
    catchArgs: string *
    catchBlock: CppStmt list
  // | IfElse of cond: CppExpr * wt: CppStmt list * wf: CppStmt list
  static let sourceMappings =
    System.Runtime.CompilerServices.ConditionalWeakTable<
      CppStmt,
      Metadata
     >()

  member this.Metadata =
    sourceMappings.TryGetValue(this)
    |> function
      | true, metadata -> Some metadata
      | false, _ -> None

let rec print (e: CppExpr) =
  match e with
  | Var s -> s
  | Const(o, ``type``) -> $"%A{o}"
  | Call(callee, args) ->
    let txtArgs =
      args |> List.map print |> String.concat ", "

    $"{print callee}({txtArgs})"
  | CallGen(callee, genArgs, args) ->
    let txtArgs =
      args |> List.map print |> String.concat ", "

    let txtGenArgs =
      genArgs |> List.map print |> String.concat ", "

    $"{print callee}<{txtGenArgs}>({txtArgs})"
  | Lambda(args, body, captures) ->
    let txtArgs =
      args
      |> List.map (fun arg -> $"auto {arg}")
      |> String.concat ", "

    let txtBody = printBody body
    let txtCaptures = String.concat ", " captures
    $"[{txtCaptures}]({txtArgs}){{{txtBody}}}"
  | GetField(src, field) -> $"{print src}.{field}"
  | DerefGetField(src, field) -> $"{print src}->{field}"
  | ExprComment c -> $"/* {c} */"

and printStmt (s: CppStmt) =
  match s with
  | SComment s -> $"/* {s} */"
  | Let(name, value) -> $"auto {name} = {print value}"
  | Exp cppExpr -> print cppExpr
  | Return cppExpr -> $"return {print cppExpr}"
  | Assign(dest, value) -> $"{print dest} = {print value}"
  | IfThenElse(cond, wt, wf) ->
    $"if ({print cond})\n{{{printBody wt}}}\nelse {{{printBody wf}}}"
  | Scope body -> $"{{{printBody body}}}"
  | SVariable(name, ty, Some value) ->
    $"{printType ty} {name} = {print value}"
  | SVariable(name, ty, None) -> $"{printType ty} {name}"
  | TryCatch(tryBlock, catchArgs, catchBlock) ->
    $"try {{{printBody tryBlock}}} catch ({catchArgs}) {{{printBody catchBlock}}}"
  | WhileLoop(cond, body) ->
    $"while ({print cond}) {{{printBody body}}}"
  | ForLoop(init, cond, post, body) ->
    $"for ({printStmt init}; {print cond}; {printStmt post}) {{{printBody body}}}"

and printBody (body: CppStmt list) =
  (body |> List.map printStmt |> String.concat ";") + ";"

and printType (ty: CppTy) =
  match ty with
  | Auto -> "auto"
  | Void -> "void"
  | Named s -> s
  | Int -> "int"
  | Bool -> "bool"
  | Gen(template, args) ->
    let txtArgs =
      args |> List.map printType |> String.concat ", "

    $"{template}<{txtArgs}>"

let private addReturn' (s: CppStmt) =
  match s with
  | WhileLoop _
  | ForLoop _
  | Scope _
  | SVariable _
  | SComment _ -> s
  | Exp cppExpr -> Return cppExpr
  | Let(name, value) -> s
  | Return cppExpr -> s
  | Assign(dest, value) -> s
  | IfThenElse(cond, wt, wf) ->
    IfThenElse(cond, addReturn wt, addReturn wf)
  | TryCatch(tryBody, args, catchBody) ->
    TryCatch(addReturn tryBody, args, addReturn catchBody)

let addReturn (stmts: CppStmt list) =
  match List.rev stmts with
  | [] -> []
  | [ last ] -> [ addReturn' last ]
  | last :: rest -> addReturn' last :: rest |> List.rev

let printDecls (decls: CppDecl list) =
  decls |> List.map printDecl |> String.concat "\n"

let printDefBody (c: CppClass) = printDecls c.decls

let printArgs (args: ArgSig) =
  args
  |> List.map (fun (name, ty) -> $"{printType ty} {name}")
  |> String.concat ", "

let printFsig (name: string) (fsig: FunctionSignature) =
  $"{printType fsig.rt} {name}({printArgs fsig.args})"

let printFimplDecl
  (name: string)
  (fsig: FunctionSignature)
  =
  $"{printType fsig.rt} {name}({printArgs fsig.args})"

let printClass (c: CppClass) =
  let txtInherits =
    if c.inherits.Length > 0 then
      let s =
        c.inherits
        |> List.map (fun s -> $"public {s}")
        |> String.concat ", "

      " : " + s
    else
      ""

  $"
class {c.name}{txtInherits} {{
public:
    {printDefBody c}
}};
"

let printStruct s =
  $"
struct {s.name} {{
public:
    {printDefBody s}
}};
"

let rec printDecl (decl: CppDecl) =
  match decl with
  | Comment s -> $"/* {s} */"
  | Variable(name, ty, None) -> $"{printType ty} {name};"
  | Variable(name, ty, Some value) ->
    $"{printType ty} {name} = {print value};"
  | Namespace(name, decls) ->
    $"namespace {name} {{\n{printDecls decls}\n}}"
  | Class c -> printClass c
  | Struct s -> printStruct s
  | Sequence decls -> printDecls decls
  | Template(args, decl) ->
    let txtArgs = String.concat ", " args
    $"template<{txtArgs}> {printDecl decl}"
  | Constructor(tyName, args, None) ->
    $"{tyName}({printArgs args});"
  | Function(name, signature, None) ->
    $"{printFsig name signature};"
  | Constructor(tyName, args, Some body) ->
    let names = args |> List.map fst

    let txtBody = shadowVariables names body |> printBody

    $"{tyName}({printArgs args}) {{ {txtBody} }}"
  | Function(name, signature, Some body) ->
    let names = signature.args |> List.map fst

    let txtBody = shadowVariables names body |> printBody

    $"{printFsig name signature} {{ {txtBody} }}"

let rec shadowVariables
  (vars: string list)
  (body: CppStmt list)
  : CppStmt list =
  match body with
  | stmt :: rest ->
    match stmt with
    | SVariable(name, ty, value) as svar ->
      if List.contains name vars then
        [
          Scope(svar :: shadowVariables (name :: vars) rest)
        ]
      else
        svar :: shadowVariables (name :: vars) rest
    | IfThenElse(cond, wt, wf) ->
      IfThenElse(
        cond,
        shadowVariables [] wt,
        shadowVariables [] wf
      )
      :: shadowVariables vars rest
    | TryCatch(tb, cargs, cb) ->
      TryCatch(
        shadowVariables [] tb,
        cargs,
        shadowVariables [] cb
      )
      :: shadowVariables vars rest
    | Scope block -> [ Scope(shadowVariables [] block) ]
    | WhileLoop(cond, body) ->
      WhileLoop(cond, shadowVariables vars body) :: rest
    | ForLoop(init, cond, post, body) ->
      // todo : add init variable to vars list
      match shadowVariables vars [ init ] with
      | init :: [] ->
        let body = shadowVariables vars body
        ForLoop(init, cond, post, body) :: rest
      | _ -> failwith "TODO Complex init for loop"
    | Let(name, value) as l ->
      if List.contains name vars then
        [ Scope(l :: shadowVariables (name :: vars) rest) ]
      else
        l :: shadowVariables (name :: vars) rest
    | Assign _
    | Return _
    | SComment _
    | Exp _ -> stmt :: shadowVariables vars rest
  | [] -> []
// let rec renameVars
//   (acc: string list)
//   (body: CppStmt list)
//   : CppStmt list =
//   let acc, stmt =
//     match body with
//     | SVariable (name, _, _) as s :: rest ->
//       if List.contains name acc then
//         // todo : rewrite variables
//         let replaced = s
//         acc, replaced
//       else
//         name :: acc, s
//     | _ -> acc
