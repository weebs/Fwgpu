module Dootverse.WebGPU.Compiler

open System.Diagnostics
open Microsoft.FSharp.Quotations
open type Quotations.Expr
open System
open System.Numerics
open Microsoft.FSharp.Core
open Microsoft.FSharp.Reflection
// open Wgsl
// open type Quotations.Expr
module Patterns = Quotations.Patterns
type Bytes =
    static let write = NativeInterop.NativePtr.write
    static let set = NativeInterop.NativePtr.set
    static member from (location: nativeptr<byte>) (offset: int) (f: float32) =
        // let ptr = NativeInterop.NativePtr.stackalloc 1
        // write ptr f
        // let mutable f = f
        // let asBytes = &&f |> NativeInterop.NativePtr.toNativeInt |> NativeInterop.NativePtr.ofNativeInt<byte>
        let asFloat = location |> NativeInterop.NativePtr.toNativeInt |> NativeInterop.NativePtr.ofNativeInt<float32>
        set asFloat offset f
        
        // NativeInterop.NativePtr.copyBlock location asBytes 4
type E = Quotations.Expr

type Fragment<'t> = Fragment of 't

type IsFragment<'t> =
    abstract member Visit<'r> : FragmentV<'t, 'r> -> 'r

and FragmentV<'t, 'r> =
    abstract member Invoke<'a> : 't -> 'a

and Fragment_<'t> = private { value: IsFragment<'t> }

and Fragment_<'t> with
    member this.Value = this.value

let createFragment'' (frag: IsFragment<'t>) : Fragment_<'t> = {
    value = frag
}

let createFragment
    (frag: Quotations.Expr<Fragment<'t -> 'u>>)
    : Fragment_<'t> =
    { value = Unchecked.defaultof<_> }

let createFragment'
    (frag: 'a -> Quotations.Expr<Fragment<'t -> 'u>>)
    : Fragment_<'t> =
    { value = Unchecked.defaultof<_> }

type Shader =
    // static member createFragment (shader: 'a -> Quotations.Expr) : Fragment_<'a> =
    //     { value = Unchecked.defaultof<_> }
    // static member createFragment (shader: 'a -> Quotations.Expr<_>) : Fragment_<'a> =
    // { value = Unchecked.defaultof<_> }
    static member createFragment
        (shader: Quotations.Expr<'a -> _>)
        : Fragment_<'a> =
        { value = Unchecked.defaultof<_> }

let runFragmentShader (frag: Fragment_<'t>) (value: 't) = ()

type WgslType =
    | Float
    | Boolean
    | Int
    | Unsigned
    | Vec3f
    | DefinedType of string

and WgslStruct = string * ((string * WgslType * string[]) list)

and WgslConst =
    | Int of int
    | Float of float32
    | Unsigned of uint32
    | Bool of bool

type WgslExpr =
    | Call of callee: string * args: WgslExpr list
    | Ident of name: string
    | PropGet of source: WgslExpr * field: string
    | IndexAccess of array: WgslExpr * index: WgslExpr
    | Value of WgslConst
    | BinaryOr of WgslExpr * WgslExpr
    | BinaryAnd of WgslExpr * WgslExpr
    | BinaryEq of WgslExpr * WgslExpr
    | Array of WgslType * WgslExpr list

type WgslStatement =
    | ExprStatement of WgslExpr
    | WhileLoop of condition: WgslExpr * body: WgslStatement list
    | VarDeclaration of name: string * value: WgslExpr option
    | LetDeclaration of name: string * value: WgslExpr
    | IfThenElse of
        cond: WgslExpr *
        whenTrue: WgslStatement list *
        whenFalse: WgslStatement list
    | Assign of var: WgslExpr * value: WgslExpr
    | ReturnExpr of expr: WgslExpr

type WgslAttr = string

type WgslModuleStatement =
    | Struct of WgslStruct
    | Alias
    | ModuleVar of
        binding: (int * int) option *
        name: string *
        types: string list *
        varType: WgslType
    | Function of
        name: string *
        args: (string * WgslType * string list) list *
        attrs: WgslAttr list *
        returning: WgslType option *
        returnAttr: string option *
        statements: WgslStatement list

and WgslFunc = {
    name: string
    args: (string * WgslType * string list) list
    attrs: WgslAttr list
    returnType: WgslType option
    returnAttr: string option
    fn: WgslStatement list
}

type WgslModuleVar = {
    name: string
    binding: (int * int) option
    varTypes: string list
    varType: WgslType
}

type Module = {
    unfinishedBindings: Map<string, WgslModuleVar>
    bindings: Map<string, WgslModuleVar>
    structs: Map<string, WgslStruct>
    fns: Map<string, WgslFunc>
} with

    static member empty = {
        unfinishedBindings = Map.empty
        bindings = Map.empty
        structs = Map.empty
        fns = Map.empty
    }

module rec Print =
    open Wgsl

    let call (callee: string) (args: WgslExpr list) =
        match callee with
        | "Sqrt" -> $"sqrt({expr args[0]})"
        | "ToUInt" -> $"u32({expr args[0]})"
        | "ToInt" -> $"i32({expr args[0]})"
        | "Cos" -> $"cos({expr args[0]})"
        | "ToInt32" -> $"i32({expr args[0]})"
        | "ToSingle" -> $"f32({expr args[0]})"
        | "GetArray" -> $"{expr args[0]}[{expr args[1]}]"
        | "SetArray" -> $"{expr args[0]}[{expr args[1]}] = {expr args[2]}"
        | "op_UnaryNegation" -> $"-{expr args[0]}"
        | "op_Multiply" -> $"({expr args[0]} * {expr args[1]})"
        | "op_Addition" -> $"({expr args[0]} + {expr args[1]})"
        | "op_Division" -> $"({expr args[0]} / {expr args[1]})"
        | "op_Modulus" -> $"({expr args[0]} %% {expr args[1]})"
        | "op_RightShift" -> $"({expr args[0]} >> u32({expr args[1]}))"
        | "op_LeftShift" -> $"({expr args[0]} << u32({expr args[1]}))"
        | "op_BitwiseAnd" -> $"({expr args[0]} & u32({expr args[1]}))"
        | "op_BitwiseOr" -> $"({expr args[0]} | u32({expr args[1]}))"
        | "op_Subtraction" -> $"({expr args[0]} - {expr args[1]})"
        | "op_GreaterThan" -> $"({expr args[0]} > {expr args[1]})"
        | "op_GreaterThanOrEqual" -> $"({expr args[0]} >= {expr args[1]})"
        | "op_Equals" -> $"({expr args[0]} == {expr args[1]})"
        | "op_Equality" -> $"({expr args[0]} == {expr args[1]})"
        | "op_Inequality" -> $"({expr args[0]} != {expr args[1]})"
        | "op_LessThan" -> $"({expr args[0]} < {expr args[1]})"
        | "op_LessThanOrEqual" -> $"({expr args[0]} <= {expr args[1]})"
        | "lessThanEqual" -> $"({expr args[0]} <= {expr args[1]})"
        | "greaterThan" -> $"({expr args[0]} > {expr args[1]})"
        | "greaterThanEqual" -> $"({expr args[0]} >= {expr args[1]})"
        | "lessThan" -> $"({expr args[0]} < {expr args[1]})"
        | _ ->
            let callArgs = String.concat ", " (List.map expr args)
            $"{callee}({callArgs})"

    let expr (expr': WgslExpr) =
        match expr' with
        | Call(callee, args) -> call callee args
        | Ident name -> name
        | PropGet(source, field) -> $"{expr source}.{field}"
        | Value wgslConst ->
            match wgslConst with
            | Int i -> $"{i}"
            | Float f ->
                // let s = $"{f}"
                let s = f.ToString("0.############")
                if s.Contains "." then s else s + ".0"
            | Unsigned u -> $"{u}u"

        | BinaryEq(wgslExpr, e) -> $"({expr wgslExpr} == {expr e})"
        | BinaryOr(wgslExpr, e) -> $"({expr wgslExpr} || {expr e})"
        | BinaryAnd(wgslExpr, e) -> $"({expr wgslExpr} && {expr e})"

        | Array(t, values) ->
            let args = List.map expr values |> String.concat ", "
            $"array<{type' t}, {values.Length}>({args})"
        | IndexAccess(wgslExpr, index) -> $"{expr wgslExpr}[{expr index}]"
        | _ -> failwith ""

    let statement (stmt: WgslStatement) =
        match stmt with
        | ExprStatement wgslExpr -> [ $"{expr wgslExpr};" ]
        | WhileLoop(condition, body) -> [
            $"while {expr condition} {{"
            yield! List.map statement body |> List.collect id
            "}"
          ]
        | VarDeclaration(name, None) -> [ $"var {name}" ]
        | VarDeclaration(name, Some value) -> [
            $"var {name} = {expr value};"
          ]
        | LetDeclaration(name, (Array _ as value)) -> [
            $"var {name} = {expr value};"
          ]
        | LetDeclaration(name, value) ->
            // [ $"let {name} = {expr value};" ]
            [ $"var {name} = {expr value};" ]
        | IfThenElse(cond, whenTrue, whenFalse) -> [
            $"if ({expr cond}) {{"
            yield! List.map statement whenTrue |> List.collect id
            $"}} else {{"
            yield! List.map statement whenFalse |> List.collect id
            "}"
          ]
        | Assign(var, value) -> [ $"{expr var} = {expr value};" ]
        | ReturnExpr e -> [ $"return {expr e};" ]

    let type' (t: WgslType) =
        match t with
        | WgslType.Float -> "f32"
        | WgslType.Boolean -> "bool"
        | WgslType.Int -> "i32"
        | WgslType.Unsigned -> "u32"
        | WgslType.Vec3f -> "vec3f"
        | WgslType.DefinedType s -> s

    let shader (compiled: WgslModuleStatement list) =
        let items = ResizeArray()

        for item in compiled do
            match item with
            | ModuleVar(binding, name, types, wgslType) ->
                let bindingStr =
                    match binding with
                    | Some(group, binding) ->
                        $"@group({group}) @binding({binding}) "
                    | None -> ""

                let varTypes =
                    if types = [] then
                        ""
                    else
                        "<" + (types |> String.concat ", ") + "> "

                let type_ = type' wgslType

                items.Add [
                    bindingStr
                    + "var"
                    + varTypes
                    + name
                    + ": "
                    + type_
                    + ";"
                ]
            | Struct(name, fields) ->
                let strStructFields =
                    fields
                    |> List.map(fun (name, t, attributes) ->
                        let attrStr =
                            if attributes.Length = 0 then
                                ""
                            else
                                (String.concat " " attributes) + " "

                        $"{attrStr}{name}: {type' t}"
                    )
                    |> String.concat ",\n    "

                items.Add [
                    $"struct {name} {{"
                    "    " + strStructFields
                    "};"
                ]
            | Alias -> ()
            | Function(name, args, attrs, returning, returnAttr, statements) ->
                let argsList =
                    args
                    |> List.map(fun (name, t, attrs) ->
                        let attr_str =
                            if attrs.Length = 0 then
                                ""
                            else
                                (attrs |> String.concat " ") + " "

                        $"{attr_str}{name}: {type' t}"
                    )
                    |> String.concat ", "

                items.Add [
                    if attrs.Length <> 0 then
                        attrs |> List.map string |> String.concat " "
                    let returnAttr =
                        match returnAttr with
                        | None -> ""
                        | Some attr -> $"{attr} "

                    let returnString =
                        match returning with
                        | None -> ""
                        | Some t -> " -> " + returnAttr + type' t

                    $"fn {name}({argsList})" + returnString + " {"

                    yield!
                        List.map Print.statement statements
                        |> List.collect id

                    "}"
                ]

        items |> Seq.map(String.concat "\n") |> String.concat "\n"

    let module' (module_: Module) =
        let items = [
            for kv in module_.structs do
                Struct kv.Value
            for kv in module_.bindings do
                ModuleVar(
                    kv.Value.binding,
                    kv.Value.name,
                    List.map string kv.Value.varTypes,
                    kv.Value.varType
                )
            for kv in module_.fns do
                Function(
                    kv.Key,
                    kv.Value.args,
                    kv.Value.attrs,
                    kv.Value.returnType,
                    kv.Value.returnAttr,
                    kv.Value.fn
                )
        ]

        shader items

open Wgsl

let rec function'
    (expr: Quotations.Expr)
    // : (string * WgslType) list * WgslStatement list * WgslType
    =
    let rec parse acc =
        function
        | Patterns.Call(_, method, [ Patterns.Value(o, _); e ]) when
            method.Name = "Location"
            ->
            // let args, statements, rt = parse ((v.Name, toType v.Type) :: acc) e
            let statements = translateStatement e
            let rt_string = Print.type'(toType e.Type)
            acc, statements, DefinedType $"@location({o}) {rt_string}"
        | Patterns.Lambda(v, e) ->
            parse
                ((v.Name, toType v.Type, [ (* todo attributes *) ]) :: acc)
                e
        | Patterns.Call(_,
                        method,
                        [ Patterns.NewUnionCase(info, _)
                          Patterns.Lambda(v, e) ]) when
            method.Name = "BuiltIn"
            ->
            let builtin =
                match info.Name with
                | "VertexIndex" -> "@builtin(vertex_index) "
                | _ -> ""

            parse
                ((builtin + v.Name,
                  toType v.Type,
                  [ (* todo attributes *) ])
                 :: acc)
                e
        | e ->
            // let args = List.rev acc |> List.map (fun v -> v.Name, toType v.Type)
            let body = translateStatement e
            List.rev acc, body, toType e.Type

    parse [] expr

and call (callee: Quotations.Expr) (arg: Quotations.Expr) =
    let rec parse acc =
        function
        | Patterns.Application(c, a) -> parse (a :: acc) c
        | Patterns.Var v -> v.Name, acc |> List.map translateExpr
        | e -> failwith $"Unrecognized pattern in call:\n{e}"

    Call(parse [ arg ] callee)

and exprType (expr: Quotations.Expr) =
    match expr with
    | Patterns.ValueWithName(o, t, name) -> t
    | Patterns.Value(o, t) -> t
    | Patterns.Lambda(var, e) ->
        FSharpType.MakeFunctionType(var.Type, exprType e)
    | Patterns.Let(var, _, _) -> var.Type
    | Patterns.IfThenElse(_, wt, _) -> exprType wt
    | Patterns.Call(_, methodInfo, _) -> methodInfo.ReturnType
    | Patterns.Application(callee, _) ->
        snd <| FSharpType.GetFunctionElements(exprType callee)
    | Patterns.FieldGet(_, info) -> info.FieldType
    | _ -> typeof<unit>

and translateExpr (expr: Quotations.Expr) =
    match expr with
    | OR conditions ->
        let rec loop =
            function
            | [ a; b ] -> BinaryOr(translateExpr a, translateExpr b)
            | a :: rest -> BinaryOr(translateExpr a, loop rest)
            | _ ->
                failwith
                    "Invalid OR result in translateExpr for Compiler.fs"

        loop conditions
    | Patterns.IfThenElse(cond, true', Patterns.Value(o, t)) when
        t = typeof<bool> && (o :?> bool) = false
        ->
        BinaryAnd(translateExpr cond, translateExpr true')
    | Patterns.UnionCaseTest(e, info) ->
        BinaryEq(PropGet(translateExpr e, "tag"), Value(Int info.Tag))
    // | Patterns.IfThenElse (Patterns.IfThenElse ifte as cond, true', Patterns.Value (o, t)) when t = typeof<bool> && (o :?> bool) = false ->
    //     match cond with
    //     | Patterns.IfThenElse (cond)
    //     BinaryAnd (translateExpr cond, translateExpr true')
    | Patterns.Var v -> Ident v.Name
    | Patterns.PropertyGet(Some a, propertyInfo, exprs) when FSharpType.IsUnion a.Type ->
        let cases = FSharpType.GetUnionCases a.Type
        let info = cases |> Array.find (fun c -> c.Name = propertyInfo.DeclaringType.Name)
        let fields = info.GetFields()
        let index = 
            fields 
            |> Array.findIndex (fun f -> f.Name = propertyInfo.Name)
        let sizeOfPreviousFields = fields |> Array.take index |> Array.map _.PropertyType |> Array.map sizeofType
        let fieldOffset = Array.sum sizeOfPreviousFields
        let this_ = translateExpr a
        translateWgslValue propertyInfo.PropertyType this_ fieldOffset
        // PropGet(translateExpr a, propertyInfo.Name)
    | Patterns.PropertyGet(Some a, propertyInfo, exprs) ->
        PropGet(translateExpr a, propertyInfo.Name)
    | Patterns.ValueWithName(o, t, name) -> // TODO this must come before Patterns.Value
        Ident name
    | Patterns.Value(o, t) ->
        match o with
        | :? int32 as i -> Value(Int i)
        | :? float32 as f -> Value(Float f)
        | :? string as s -> Value(Unsigned 4205731365u)
        | :? uint as u -> Value(Unsigned u)
        | :? bool as b -> Value(Bool b)
        | _ -> failwith $"translateExpr: Cannot translate value {o}"
    | Patterns.Application(callee, arg) -> call callee arg
    | Patterns.NewUnionCase(caseInfo, values) ->
        failwith
            $"TODO translateExpr Union: {caseInfo.DeclaringType.FullName}\n%A{values}"
    | Patterns.NewArray(arrayType, values) ->
        Array(toType arrayType, List.map translateExpr values)
    | Patterns.NewRecord(t, values) ->
        Call(t.Name, List.map translateExpr values)
    | Patterns.Coerce(value, t) ->
        if t = typeof<int> then
            Call("i32", [ translateExpr value ])
        elif t = typeof<float32> then
            Call("f32", [ translateExpr value ])
        else
            failwith $""
    | Patterns.Call(thisArg, methodInfo, args) ->
        Call(methodInfo.Name, List.map translateExpr args)
    | Patterns.FieldGet(Some(Patterns.Var v), info) when v.Name = "this" ->
        Ident info.Name
    | Patterns.DefaultValue t -> Ident $"default<{Print.type'(toType t)}>"
    | _ -> failwith $"Unrecognized pattern in translateExpr: {expr}"

and translateWgslValue t this_ offset =
    if t = typeof<vec3<float32>> then
        let args = [
            IndexAccess(PropGet(this_, "data"), Value(Int <| offset + 0))
            IndexAccess(PropGet(this_, "data"), Value(Int <| offset + 1))
            IndexAccess(PropGet(this_, "data"), Value(Int <| offset + 2))
        ]

        Call("vec3f", args)
    elif t = typeof<float32> then
        IndexAccess(PropGet(this_, "data"), Value(Int offset))
    elif t = typeof<int> then
        Call(
            "bitcast<i32>",
            [ IndexAccess(PropGet(this_, "data"), Value(Int offset)) ]
        )
    else
        failwith ""

and caseSize (c: UnionCaseInfo) =
    let fields = c.GetFields()

    fields
    |> Array.map(_.PropertyType >> sizeofType)
    |> Array.reduce (+)

and sizeofType (t: Type) =
    if FSharpType.IsRecord t then
        let fields = FSharpType.GetRecordFields t
        fields |> Array.map(_.PropertyType >> sizeofType) |> Array.sum
    elif t = typeof<vec3<float32>> then
        3
    elif t = typeof<float32> then 1
    elif t = typeof<int> then 1
    elif t = typeof<uint> then 1
    elif FSharpType.IsUnion t then
        FSharpType.GetUnionCases t
        |> Array.map caseSize
        |> Array.max
        |> (+) 1 // + 1 for the tag field
    else failwith $"Couldn't calculate size of type {t.FullName}"

and serializeObj (o: obj) =
    match o with
    | :? int as i -> BitConverter.GetBytes i
    | :? uint as u -> BitConverter.GetBytes u
    | :? single as s -> BitConverter.GetBytes s
    | :? vec3f as v -> [|
        yield! BitConverter.GetBytes v.x
        yield! BitConverter.GetBytes v.y
        yield! BitConverter.GetBytes v.z
      |]
    // | o when FSharpType.IsRecord (o.GetType()) ->
    // let fields =
    // | :? double as f -> BitConverter.GetBytes f
    | _ ->
        Debugger.Break()
        failwith ""
// let makeSerialize<'t> (t: System.Type) =
and makeSerialize<'t> () =
    let t = typeof<'t>

    if t = typeof<int32> then
        fun (i: 't) -> BitConverter.GetBytes(box i :?> int32)
    elif t = typeof<float32> then
        fun (i: 't) -> BitConverter.GetBytes(box i :?> float32)
    elif t = typeof<uint> then
        fun (i: 't) -> BitConverter.GetBytes(box i :?> uint)
    elif t = typeof<vec3f> then
        fun (i: 't) ->
            let value = box i :?> vec3f

            [|
                yield! BitConverter.GetBytes value.x
                yield! BitConverter.GetBytes value.y
                yield! BitConverter.GetBytes value.z
            |]
    elif t = typeof<vec4<float32>> then
        
        fun (i: 't) ->
            let value = box i :?> vec4<float32>

            let result = Array.zeroCreate 16
            use ptr = fixed result
            Bytes.from ptr 0 value.x
            Bytes.from ptr 1 value.y
            Bytes.from ptr 2 value.z
            Bytes.from ptr 3 value.w
            result
    elif FSharpType.IsRecord t then
        let fields = FSharpType.GetRecordFields t

        fun (o: 't) -> [|
            for field in fields do
                let value = field.GetValue(o)
                yield! serializeObj value
        |]
    elif FSharpType.IsUnion t then
        let cases = FSharpType.GetUnionCases t

        let maxCaseSize = cases |> Array.map(caseSize) |> Array.max

        fun (o: 't) ->
            let (info, fields) = FSharpValue.GetUnionFields(o, t)

            let result = [|
                for field in fields do
                    yield! serializeObj field
            |]

            let remaining = maxCaseSize * 4 - result.Length

            [|
                yield! BitConverter.GetBytes info.Tag
                yield! result
                yield! Array.zeroCreate remaining
            |]
    else
        Debugger.Break()
        failwith ""

and (|UnionLet|_|) e =
    match e with
    | Patterns.Let(variable,
                   Patterns.PropertyGet(Some this, prop, args),
                   rest) ->
        // when FSharpType.IsUnion this.Type && prop.Name.StartsWith "Item" ->
        let isUnion = FSharpType.IsUnion this.Type
        let startsWithItem = prop.Name.StartsWith "Item"

        if isUnion && startsWithItem then
            let index = Int32.Parse(prop.Name.Substring("Item".Length))
            Some(variable, index, rest)
        else
            None
    | _ -> None

and getLetExprs e =
    let rec loop acc e =
        match e with
        | UnionLet(variable, index, rest) ->
            let acc = (variable, index) :: acc

            match rest with
            | UnionLet _ -> loop acc rest
            | _ -> List.rev acc, rest
        | _ -> List.rev acc, e

    loop [] e

and (|MatchExpr|_|) (statement: Quotations.Expr) =
    match statement with
    | Patterns.IfThenElse(Patterns.UnionCaseTest(union, info),
                          ifCase,
                          else_) ->
        // Some (union, info, getLetExprs ifCase, else_)
        Some(union, info, getLetExprs ifCase, else_)
    | _ -> None

and getMatchExprs statement =
    let rec loop acc (statement: Expr) =
        match statement with
        // | MatchExpr (union, info, ifCase, else_) ->
        | MatchExpr(union, info, ifCase, else_) ->
            // let acc = ((union, info, ifCase) :: acc)
            // let acc = ((info.Tag, ifCase) :: acc)
            let acc = (info.Tag, ifCase) :: acc

            match else_ with
            | MatchExpr _ -> loop acc else_
            | UnionLet _ ->
                let e = getLetExprs else_
                let nums = acc |> List.map fst |> List.toArray
                let cases = FSharpType.GetUnionCases info.DeclaringType

                let missing =
                    [|
                        for i in 0 .. cases.Length - 1 do
                            if Array.contains i nums = false then
                                i
                    |]
                    |> Array.head
                // let nums =
                // let missing =
                Some(
                    union,
                    info.DeclaringType,
                    acc @ [ (missing, e) ],
                    None
                )
            // Some (acc, else_)
            | _ -> Some(union, info.DeclaringType, acc, Some else_)
        | _ ->
            // [], statement
            None

    loop [] statement

and simpleExpr (e: Quotations.Expr) =
    match e with
    | Patterns.ForIntegerRangeLoop _ -> false
    | Patterns.WhileLoop _ -> false
    | Patterns.Call(thisArg, method, args) ->
        let anyNonSimple = args |> List.exists(not << simpleExpr)

        match thisArg, anyNonSimple with
        | (Some(SimpleExpr _) | None), false -> true
        | _ -> false
    | OR _ -> true
    | Patterns.IfThenElse _ -> false
    | Patterns.FieldSet _ -> false
    | Patterns.Sequential _ -> false
    | Patterns.VarSet _ -> false
    | Patterns.TryFinally _ -> false
    | Patterns.TryWith _ -> false
    | _ -> true

and (|SimpleExpr|_|) (statement: Quotations.Expr) =
    if simpleExpr statement then Some statement else None

and (|StatementExpr|_|) (statement: Quotations.Expr) =
    if simpleExpr statement = false then
        Some statement
    else
        None

and (|DecisionTree|_|) (statement: Quotations.Expr) =
    getMatchExprs statement
// match statement with
// | MatchExpr _ ->
//     let acc, e = getMatchExprs statement
//     Some (acc, e)
// | _ ->
//     None
and (|OR|_|) (e: Quotations.Expr) =
    match e with
    | Patterns.IfThenElse(SimpleExpr e1, True, SimpleExpr else_) ->
        Some [ e1; else_ ]
    | Patterns.IfThenElse(SimpleExpr e1, True, OR b) ->
        Some [ e1; yield! b ]
    // TODO
    // | Patterns.IfThenElse(OR _, True, OR b) ->
    //     Some [ e; yield! b ]
    // | Patterns.IfThenElse(OR _, True, SimpleExpr else_) ->
    //     Some [ e; yield! b ]
    | _ -> None

and (|True|_|) (e: Quotations.Expr) =
    match e with
    | Patterns.Value(o, _) ->
        match o with
        | :? bool as b when b = true -> Some()
        | _ -> None
    | _ -> None

and (|False|_|) (e: Quotations.Expr) =
    match e with
    | Patterns.Value(o, _) ->
        match o with
        | :? bool as b when b = true -> Some()
        | _ -> None
    | _ -> None

and simplify (statement: Quotations.Expr) =
    match statement with
    | Patterns.IfThenElse(SimpleExpr cond, True, False) -> cond
    | _ -> statement

and translateStatement (statement: Quotations.Expr) = translateS statement

and private translateS (statement: Quotations.Expr) =
    match statement with
    | DecisionTree(union, ut, acc, e) ->
        let cases = FSharpType.GetUnionCases ut
        let unionExpr = translateExpr union

        let createBindings
            (tag: int)
            (bindings: (Quotations.Var * int) list)
            =
            let caseFields = cases[tag].GetFields()

            let caseSizes =
                caseFields |> Array.map(_.PropertyType >> sizeofType)

            let caseOffsets =
                caseSizes
                |> Array.mapi(fun index _ ->
                    Array.sum(Array.take index caseSizes)
                )

            printfn $""

            (bindings, [])
            ||> List.foldBack(fun (var, item) acc ->
                let offset = caseOffsets[item - 1]

                LetDeclaration(
                    var.Name,
                    translateWgslValue
                        caseFields[item - 1].PropertyType
                        unionExpr
                        offset
                )
                :: acc
            )

        let rec loop =
            function
            | (tag, (bindings, expr)) :: [] ->
                let cond =
                    BinaryEq(
                        PropGet(translateExpr union, "tag"),
                        Value(Int tag)
                    )

                match e with
                | Some e -> [
                    IfThenElse(
                        cond,
                        createBindings tag bindings @ translateS expr,
                        translateS e
                    )
                  ]
                | None ->
                    let letStatements = createBindings tag bindings
                    let condStatements = translateS expr
                    letStatements @ condStatements
            | (tag, (bindings, expr)) :: rest ->
                let cond =
                    BinaryEq(
                        PropGet(translateExpr union, "tag"),
                        Value(Int tag)
                    )

                [
                    IfThenElse(
                        cond,
                        createBindings tag bindings @ translateS expr,
                        loop rest
                    )
                ]

        let result = loop acc
        // ((), [ 1; 2; 3; 4; 5 ]) ||> List.fold (fun _ value -> printfn $"{value}")
        result
    | Patterns.PropertySet(Some o, prop, args, value) -> [
        Assign(PropGet(translateExpr o, prop.Name), translateExpr value)
      ]
    // | Patterns.Application (callee, arg) -> []
    | Patterns.Let(variable, value, e) ->
        match value with
        // | Patterns.PropertyGet (Some this, prop, args)
        //   when FSharpType.IsUnion this.Type && prop.Name.StartsWith "Item" ->
        //     let info = FSharpType.GetUnionCases this.Type
        //     let index = Int32.Parse (prop.Name.Substring("Item".Length))
        //     let offset = 0
        //     let this_ = translateExpr this
        //     translatedWgslValue  @ translateStatement e
        | Patterns.Lambda(var, absExpr) -> [] // TODO lambda
        | _ ->
            if variable.IsMutable then
                VarDeclaration(variable.Name, Some(translateExpr value))
                :: translateS e
            else
                LetDeclaration(variable.Name, translateExpr value)
                :: translateS e
    | Patterns.Call(thisArg, method, args) when
        method.Name = "VertexShader"
          ->
          []
    | Patterns.IfThenElse(Patterns.UnionCaseTest(union, info) as cond,
                          ifCase,
                          else_) ->
        let createVariables
            (info: UnionCaseInfo)
            (bindings: (Quotations.Var * int) list)
            =
            let fieldSizes =
                info.GetFields() |> Array.map(_.PropertyType >> sizeofType)

            let fieldOffsets =
                fieldSizes
                |> Array.mapi(fun i _ ->
                    Array.sum(Array.take i fieldSizes)
                )

            [
                // for (var, caseTag) in bindings do
                for index in 0 .. bindings.Length - 1 do
                    let (var, caseTag) = bindings[index]
                    // let t = info.GetFields()[caseTag]
                    LetDeclaration(
                        var.Name,
                        translateWgslValue
                            var.Type
                            (translateExpr union)
                            fieldOffsets[index]
                    )
            ]

        let rec loop e acc =
            match e with
            | UnionLet(variable, index, rest) ->
                loop rest ((variable, index) :: acc)
            | _ -> List.rev acc, e

        let bindings, rest = loop ifCase []
        let variables = createVariables info bindings
        let result = variables @ translateS rest
        let elseVars, elseBranch = loop else_ []
        // todo if the elseVars list is not empty, translate the union cases there too!
        if elseVars.Length = 0 then
            [ IfThenElse(translateExpr cond, result, translateS else_) ]
        else
            let variables' = createVariables info elseVars
            let result' = variables' @ translateS rest

            [
                IfThenElse(
                    translateExpr cond,
                    result,
                    result' @ translateS elseBranch
                )
            ]
    | Patterns.IfThenElse(SimpleExpr e, True, SimpleExpr else_) ->
        let result =
            ExprStatement(BinaryOr(translateExpr e, translateExpr else_))

        [ result ]
    | Patterns.IfThenElse(cond, true', false') -> [
        IfThenElse(translateExpr cond, translateS true', translateS false')
      ]
    | Patterns.WhileLoop(cond, loop) -> [
        WhileLoop(translateExpr cond, translateS loop)
      ]
    | Patterns.Sequential(e, e') -> translateS e @ translateS e'
    | Patterns.VarSet(var, value) -> [
        Assign(Ident var.Name, translateExpr value)
      ]
    | Patterns.Value(null, t) when t = typeof<unit> -> []
    | e -> [ ExprStatement(translateExpr e) ]

and getLambdaExprReturn e =
    match e with
    | Patterns.Lambda(_, Patterns.Lambda(_, e')) -> getLambdaExprReturn e'
    | _ -> e.Type

and parseVarTypes (expr: Quotations.Expr) =
    let convertName =
        function
        | "ReadWrite" -> "read_write"
        | s -> s.ToLower()

    let quote =
        <@
            let (|Foo|) (a, b: int) = if a % 2 = 0 then 0, 2 else 1, 2
            let doot (Foo(value, b)) = ()

            match 1234, 2 with
            | Foo(a, b) -> a
            | _ -> 0
        @>

    let rec loop e acc =
        match e with
        | Patterns.NewUnionCase(info,
                                [ Patterns.NewUnionCase(t, _); value2 ]) when
            info.Name = "Cons"
            ->
            loop value2 (convertName t.Name :: acc)
        | _ -> List.distinct acc |> List.rev

    loop expr []

and translateModuleItem (item: Quotations.Expr) (module_: Module) =
    // and translateModule (module_: Quotations.Expr<Fragment<'t -> 'u>>) =
    match item with
    // | Patterns.Let (v, Patterns.Call (_, methodA, [ Patterns.Call (thisArg, methodB, [ builtinType; Patterns.Lambda (lambdaVar, lambdaExpr) ]) ]), following)
    //     when methodA.Name = "VertexShader" && methodB.Name = "BuiltIn" ->
    //     match builtinType with
    //     | Patterns.NewUnionCase (info, _) ->
    //         let builtin =
    //             match info.Name with
    //             | "VertexIndex" -> "@builtin(vertex_index)"
    //         printfn $"{info}"
    //         let args, abs = function' lambdaVar lambdaExpr
    //         let returnType = toType lambdaExpr.Type
    //         // let rt_string = Print.type' returnType
    //         Function (v.Name, args, [ "vertex" ], Some returnType, addReturn abs)
    //         :: translateModule following
    | Patterns.Let(var, Patterns.Var v, e) when var.Name = v.Name ->
        translateModuleItem e module_
    | Patterns.Let(var,
                   Patterns.Call(_, method, [ varTypes; Patterns.Var v ]),
                   e) when method.Name = "Var" ->
        let binding = module_.unfinishedBindings[v.Name]
        // { module_ with bindings }
        {
            module_ with
                bindings =
                    module_.bindings.Add(
                        var.Name,
                        {
                            name = var.Name
                            binding = binding.binding
                            // binding = Some (0, module_.bindings.Count)
                            varTypes = parseVarTypes varTypes
                            varType = binding.varType
                        }
                    )
        }
        |> translateModuleItem e
    | Patterns.Let(v, (Patterns.Lambda(arg1, absExpr) as l), following) ->
        let args, abs, t' = function' l
        // let rt_ = getLambdaExprReturn absExpr
        let rt = Some t'
        // let rt = match rt_ with | t when t = typeof<unit> -> None | t -> Some (toType t)
        {
            module_ with
                fns =
                    module_.fns.Add(
                        v.Name,
                        {
                            name = v.Name
                            returnAttr = None
                            args = args
                            attrs = []
                            returnType = rt
                            fn = addReturn abs
                        }
                    )
        }
        // Function (v.Name, args, [], rt, addReturn abs) ::
        |> translateModuleItem following
    // | Patterns.Let (v, Patterns.Call (None, method, [ (Patterns.Lambda (lambdaVar, lambdaExpr) as l) ]), following)
    // | Patterns.Let (v, Patterns.Call (None, method, [ (Patterns.Lambda (lambdaVar, lambdaExpr) as l) ]), following)
    // when method.Name = "VertexShader" ->
    | Patterns.Let(v, Patterns.Call(None, method, [ l ]), following)
    | Patterns.Let(v, Patterns.Call(None, method, [ l ]), following) when
        method.Name = "VertexShader" || method.Name = "FragmentShader"
        ->
        let args, abs, t' = function' l
        // let rt = match exprType lambdaExpr with | t when t = typeof<unit> -> None | t -> Some (toType t)
        // let returnType = toType t'
        let rt_string = Print.type' t'
        let rt = DefinedType $"{rt_string}"
        // Function (v.Name, args, [ if method.Name = "VertexShader" then "vertex" else "fragment" ], Some rt, addReturn abs) ::
        {
            module_ with
                fns =
                    module_.fns.Add(
                        v.Name,
                        {
                            name = v.Name
                            returnAttr = None
                            args = args
                            attrs = [
                                if method.Name = "VertexShader" then
                                    "@vertex"
                                else
                                    "@fragment"
                            ]
                            returnType = Some rt
                            fn = addReturn abs
                        }
                    )
        }
        |> translateModuleItem following
    | Patterns.Value(null, t) when t = typeof<unit> -> module_
    | _ -> module_

and translateStruct (t: Type) =
    if FSharpType.IsRecord t then
        let fields = FSharpType.GetRecordFields t

        t.Name,
        fields |> Array.map(fun f -> f.Name, toType f.PropertyType),
        []
    else
        let fields = t.GetFields()

        t.Name,
        fields |> Array.map(fun f -> f.Name, toType f.FieldType),
        []
// structsUsedByType t

and requiresDecl (t: Type) =
    if t = typeof<int> then false
    elif t = typeof<float32> then false
    elif t = typeof<uint> then false
    elif t = typeof<int[]> then false
    elif t = typeof<float32[]> then false
    elif t = typeof<uint[]> then false
    else true

and translateModule (expr: Quotations.Expr) (module_: Module) : Module =
    let rec getModule (e: Quotations.Expr) acc =
        match e with
        | Patterns.Let(var, Patterns.TupleGet(value, index), expr) ->
            (acc @ [ var, value.Type.GenericTypeArguments[index] ])
            |> getModule expr
        | _ -> e, acc

    match expr with
    | Patterns.Lambda(var, e) ->
        let m, acc = (getModule e [])

        let structs =
            acc
            |> List.filter(snd >> requiresDecl)
            |> List.map(snd >> structsUsedByType >> List.ofArray)
            |> List.collect id
        // |> List.concat (gatherStructs m |> List.ofArray)
        let moreStructs = gatherStructs m |> List.ofArray

        let newStructs =
            (module_.structs, structs @ moreStructs |> List.distinct)
            ||> List.fold(fun acc struct_ -> acc.Add(fst struct_, struct_))
        // |> Map.ofList
        let bindings = [
            for i in 0 .. acc.Length - 1 do
                // ModuleVar (Some (0, i), (fst acc[i]).Name, [ "uniform" ], toType (snd acc[i]))
                let name = (fst acc[i]).Name

                name,
                {
                    name = name
                    binding = Some(0, i)
                    varTypes = []
                    varType = toType(snd acc[i])
                }
        ]
        // structs @ bindings @ translateModuleItem m
        // let newStructs =
        {
            module_ with
                structs = newStructs
                unfinishedBindings = Map.ofList bindings
        }
        |> translateModuleItem m

and toType (t: System.Type) =
    match t with
    | t when t = typeof<int> -> WgslType.Int
    | t when t = typeof<uint> -> WgslType.Unsigned
    | t when t = typeof<float32> -> WgslType.Float
    | t when t = typeof<bool> -> WgslType.Boolean
    // | t when t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<vec2<_>> ->
    // Wgsl.DefinedType
    | t when t = typeof<Wgsl.vec2<float32>> -> WgslType.DefinedType "vec2f"
    | t when t = typeof<Wgsl.vec3<float32>> -> WgslType.DefinedType "vec3f"
    | t when t = typeof<Wgsl.vec4<float32>> -> WgslType.DefinedType "vec4f"
    | t when t = typeof<Wgsl.vec2<int>> -> WgslType.DefinedType "vec2<i32>"
    | t when t = typeof<Wgsl.vec3<int>> -> WgslType.DefinedType "vec3<i32>"
    | t when t = typeof<Wgsl.vec4<int>> -> WgslType.DefinedType "vec4<i32>"
    | t when t = typeof<Wgsl.vec2<uint>> ->
        WgslType.DefinedType "vec2<u32>"
    | t when t = typeof<Wgsl.vec3<uint>> ->
        WgslType.DefinedType "vec3<u32>"
    | t when t = typeof<Wgsl.vec4<uint>> ->
        WgslType.DefinedType "vec4<u32>"
    | t when t.IsArray ->
        let elementType =
            t.GetMethods()
            |> Array.find(_.Name >> (=) "Get")
            |> _.ReturnType

        WgslType.DefinedType $"array<{toType elementType |> Print.type'}>"
    | _ -> WgslType.DefinedType t.Name

and addReturn (statements: WgslStatement list) =
    let result =
        match List.last statements with
        | ExprStatement wgslExpr -> Some(ReturnExpr wgslExpr)
        | WhileLoop(condition, body) -> None
        | VarDeclaration(name, value) -> None
        | LetDeclaration(name, value) -> None
        | IfThenElse(cond, whenTrue, whenFalse) ->
            Some
            <| IfThenElse(cond, addReturn whenTrue, addReturn whenFalse)
        | Assign(var, value) -> None
        | ReturnExpr expr -> None

    match result with
    | None -> statements
    | Some s -> List.take (statements.Length - 1) statements @ [ s ]

and visitExpr
    (fn: Quotations.Expr -> Quotations.Expr)
    (e: Quotations.Expr)
    =
    match e with
    | Patterns.Call(this, method, args) ->
        if this <> None then
            E.Call(this.Value, method, List.map fn args)
        else
            E.Call(method, List.map fn args)
    | Patterns.Lambda(var, expr) -> E.Lambda(var, fn expr)
    | Patterns.WhileLoop(cond, expr) -> E.WhileLoop(fn cond, fn expr)
    | Patterns.IfThenElse(cond, whenTrue, whenFalse) ->
        E.IfThenElse(fn cond, fn whenTrue, fn whenFalse)
    | Patterns.Let(var, value, e) -> E.Let(var, fn value, fn e)
    | expr -> expr

and visitInner f e = (visitExpr (visitInner f) e) |> f
// visitExpr f e |> visitExpr (visitInner f)

and visitOuter f e =
    match f e with
    | Some e -> e
    | None -> visitExpr (visitOuter f) e
// let (|Asdf|_|) a = if a = 0 then Some 1234 else None
// let Asdf = function Asdf value -> Some value | _ -> None
and (|WgslStruct|_|) (t: Type) : WgslStruct option =
    if FSharpType.IsRecord t && not t.IsGenericType then
        let fields = FSharpType.GetRecordFields t

        let result =
            fields
            |> Array.map(fun f ->
                let attributes =
                    f.CustomAttributes
                    |> Seq.toArray
                    |> Array.choose(fun data ->
                        let c =
                            data.Constructor.Invoke(
                                data.ConstructorArguments
                                |> Seq.map _.Value
                                |> Seq.toArray
                            )

                        match c with
                        | :? Wgsl.WgslAttribute as attr ->
                            Some attr.Serialize
                        | _ -> None
                    )

                f.Name, toType f.PropertyType, attributes
            )

        Some(t.Name, (List.ofArray result))
    elif
        FSharpType.IsUnion t
        && not(t.Name.Contains "List")
        && not(t = typeof<Builtin>)
    then
        let caseSizes =
            FSharpType.GetUnionCases(t)
            |> Array.map(_.GetFields())
            |> Array.map(Array.map(_.PropertyType >> sizeofType))
            |> Array.map Array.sum

        let sizeOfStruct = Array.max caseSizes

        Some(
            t.Name,
            [
                "tag", WgslType.Int, [||]
                "data",
                WgslType.DefinedType $"array<f32, {sizeOfStruct}>",
                [||]
            ]
        )
    else
        None

and structsUsedByType (t: Type) : WgslStruct array =
    if FSharpType.IsTuple t then
        FSharpType.GetTupleElements t
        |> Array.map structsUsedByType
        |> Array.collect id
    elif FSharpType.IsFunction t then
        let a, b = FSharpType.GetFunctionElements t
        structsUsedByType a |> Array.append(structsUsedByType b)
    elif t.IsArray then
        let argType =
            t.GetMethods()
            |> Array.find(fun m -> m.Name = "Get")
            |> _.ReturnParameter.ParameterType

        match argType with
        | WgslStruct fields -> [| fields |]
        | _ -> [||]
    else
        match t with
        | WgslStruct fields -> [| fields |]
        | _ -> [||]

and structsUsed (e: Quotations.Expr) =
    let inline else_ (e: Quotations.Expr) = structsUsedByType e.Type
    // match e.Type with
    // | WgslStruct (name, fields) -> [| name, fields |]
    // | _ -> [||]
    match e with
    | Patterns.Lambda(var, _) -> structsUsedByType var.Type
    | Patterns.Call(thisArg, method, _) ->
        let types =
            method.GetParameters()
            |> Array.map _.ParameterType
            |> Array.append [| method.ReturnType |]
            |> Array.append [|
                if thisArg <> None then
                    thisArg.Value.Type
            |]

        types |> Array.map structsUsedByType |> Array.collect id
    | _ -> else_ e

and gatherStructs e =
    let structs = ResizeArray()

    visitOuter
        (fun e ->
            structs.AddRange(structsUsed e)
            // e
            None
        )
        e
    |> ignore

    structs |> Seq.distinct |> Seq.toArray
// exprModule.CustomAttributes

module Reify =
    type Expr =
        | Call of callee: string * args: Expr list
        | IfThenElse of cond: Expr * whenTrue: Expr * whenFalse: Expr
        | Let of var: Quotations.Var * value: Expr * e: Expr
        | WhileLoop of cond: Expr * loop: Expr

    let rec reify (quote: Quotations.Expr) =
        match quote with
        | Patterns.WhileLoop(cond, loop) ->
            WhileLoop(reify cond, reify loop)
        | Patterns.Let(var, value, expr) ->
            Let(var, reify value, reify expr)
        | Patterns.IfThenElse(cond, wt, wf) ->
            IfThenElse(reify cond, reify wt, reify wf)
        | Patterns.Call(this, method, args) ->
            Expr.Call(method.Name, List.map reify args)
        | _ -> failwith($"{quote}\n" + (__SOURCE_FILE__ + ":" + __LINE__))
