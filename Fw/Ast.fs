module rec Fw.Ast

open FSharp.Compiler.Symbols

type Metadata = class end

type CppTy =
    | Int
    | Void
    | Named of string
    | Auto
    | Gen of template: string * args: CppTy list
    
type CppClass =
    {
        name: string
        inherits: string list
        decls: CppDecl list
        fields: (string * CppTy) list
        constructors: ((string * CppTy) list * CppStmt list) list
    }
    
type CppStruct = CppClass

type CppDecl =
    // | Constructor of args: (string * CppTy) list * body: CppStmt list
    | Namespace of name: string * decls: CppDecl list
    | Template of args: string * decl: CppDecl
    | Class of CppClass
    | Function of name: string * args: (string * CppTy) list * rt: CppTy * body: CppStmt list
    | Struct of CppStruct
    | Comment of string
    | Variable of name: string * ty: CppTy * value: CppExpr option
    
type CppExpr =
    | Var of string
    | Const of obj * FSharpType
    | Call of callee: CppExpr * args: CppExpr list
    
    | Lambda of args: string list * body: CppStmt list
    | GetField of src: CppExpr * field: string
    static let sourceMappings = System.Runtime.CompilerServices.ConditionalWeakTable<CppExpr, Metadata>()

    member this.Metadata =
        sourceMappings.TryGetValue(this)
        |> function
            | true, metadata -> Some metadata
            | false, _ -> None
            
and CppStmt =
    | Let of name: string * value: CppExpr
    | Exp of CppExpr
    | Return of CppExpr
    | Assign of dest: CppExpr * value: CppExpr
    // | IfElse of cond: CppExpr * wt: CppStmt list * wf: CppStmt list
    static let sourceMappings = System.Runtime.CompilerServices.ConditionalWeakTable<CppStmt, Metadata>()
        
    member this.Metadata =
        sourceMappings.TryGetValue(this)
        |> function
            | true, metadata -> Some metadata
            | false, _ -> None
    
let rec print (e: CppExpr) =
    match e with
    | Var s ->
        s
    | Const(o, ``type``) ->
        $"%A{o}"
    | Call(callee, args) ->
        let txtArgs = args |> List.map print |> String.concat ", "
        $"{print callee}({txtArgs})"
    | Lambda(args, body) ->
        let txtArgs = args |> List.map (fun arg -> $"auto {arg}") |> String.concat ", "
        let txtBody = printBody body
        $"[]({txtArgs}){{{txtBody}}}"
    | GetField(src, field) -> $"{print src}.{field}"
    
and printStmt (s: CppStmt) =
    match s with
    | Let(name, value) ->
        $"auto {name} = {print value}"
    | Exp cppExpr ->
        print cppExpr
    | Return cppExpr ->
        $"return {print cppExpr}"
    | Assign(dest, value) ->
        $"{print dest} = {print value}"
        
and printBody (body: CppStmt list) =
    (body |> List.map printStmt |> String.concat ";")
    + ";"
    
and printType (ty: CppTy) =
    match ty with
    | Auto -> "auto"
    | Void -> "void"
    | Named s -> s
    | Int -> "int"
    | Gen(template, args) ->
        let txtArgs = args |> List.map printType |> String.concat ", "
        $"{template}<{txtArgs}>"

let private addReturn' (s: CppStmt) =
    match s with
    | Exp cppExpr -> Return cppExpr
    | Let(name, value) -> s
    | Return cppExpr -> s
    | Assign(dest, value) -> s
    
let addReturn (stmts: CppStmt list) =
    match List.rev stmts with
    | [] -> []
    | [ last ] -> [ addReturn' last ]
    | last :: rest -> addReturn' last :: rest |> List.rev
        
let rec printDecl (decl: CppDecl) =
    match decl with
    // | Constructor(args, body) -> failwith "todo"
    | Namespace(name, decls) ->
        let inner = decls |> List.map printDecl |> String.concat "\n"
        $"namespace {name} {{\n{inner}\n}}"
    | Template(args, decl) -> failwith "todo"
    | Class c ->
        let inner = c.decls |> List.map printDecl |> String.concat "\n"
        $"
class {c.name} {{
{inner}
}};
"
    | Function(name, args, rt, body) ->
        let srt = printType rt
        let inner = printBody body
        let sargs = args |> List.map (fun (name, ty) -> $"{printType ty} {name}") |> String.concat ", "
        $"{srt} {name}({sargs}) {{ {inner} }}"
    | Struct s ->
        let inner = s.decls |> List.map printDecl |> String.concat "\n"
        $"
struct {s.name} {{
{inner}
}};
"
    | Comment s -> $"/* {s} */"
    | Variable(name, ty, None) ->
        $"{printType ty} {name};"
    | Variable(name, ty, Some value) ->
        $"{printType ty} {name} = {print value};"
