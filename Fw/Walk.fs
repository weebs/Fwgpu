module Fw.Walk

open FSharp.Compiler.Symbols

type Ty = FSharpType
type Mfv = FSharpMemberOrFunctionOrValue
type S =
    | AddressOf of lvalue: S
    | AddressSet of lvalue: S * rvalue: S
    | Application of f: S * tys: Ty list * args: S list
    | Call of o: S option * mfv: Mfv * tyArgs1: Ty list * tyArgs2: Ty list * args: S list
    | Coerce of target: Ty * e: S
    | FastIntegerForLoop of start: S * limit: S * consume: S * isUp: bool
    | ILAsm
    | ILFieldGet
    | ILFieldSet
    | IfThenElse of guard: S * wt: S * wf: S
    | Lambda of var: Mfv * body: S
    | Let of binding: Mfv * value: S * body: S

let rec visitExpr f (n: int) (e:FSharpExpr) = 
    f n e
    match e with 
    | FSharpExprPatterns.AddressOf(lvalueExpr) -> 
        visitExpr f (n + 1) lvalueExpr
    | FSharpExprPatterns.AddressSet(lvalueExpr, rvalueExpr) -> 
        visitExpr f (n + 1) lvalueExpr; visitExpr f (n + 1) rvalueExpr
    | FSharpExprPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        visitExpr f (n + 1) funcExpr; visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        visitObjArg f (n + 1) objExprOpt; visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.Coerce(targetType, inpExpr) -> 
        visitExpr f (n + 1) inpExpr
    | FSharpExprPatterns.FastIntegerForLoop(startExpr, limitExpr, consumeExpr, isUp, _, _) -> 
        visitExpr f (n + 1) startExpr; visitExpr f (n + 1) limitExpr; visitExpr f (n + 1) consumeExpr
    | FSharpExprPatterns.ILAsm(asmCode, typeArgs, argExprs) -> 
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.ILFieldGet (objExprOpt, fieldType, fieldName) -> 
        visitObjArg f (n + 1) objExprOpt
    | FSharpExprPatterns.ILFieldSet (objExprOpt, fieldType, fieldName, valueExpr) -> 
        visitObjArg f (n + 1) objExprOpt
    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
        visitExpr f (n + 1) guardExpr; visitExpr f (n + 1) thenExpr; visitExpr f (n + 1) elseExpr
    | FSharpExprPatterns.Lambda(lambdaVar, bodyExpr) -> 
        visitExpr f (n + 1) bodyExpr
    | FSharpExprPatterns.Let((bindingVar, bindingExpr, dbg), bodyExpr) -> 
        visitExpr f (n + 1) bindingExpr; visitExpr f (n + 1) bodyExpr
    | FSharpExprPatterns.LetRec(recursiveBindings, bodyExpr) ->
        for _,bindingExpr,_ in recursiveBindings do visitExpr f (n + 1) bindingExpr
        visitExpr f (n + 1) bodyExpr
    | FSharpExprPatterns.NewArray(arrayType, argExprs) -> 
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.NewDelegate(delegateType, delegateBodyExpr) -> 
        visitExpr f (n + 1) delegateBodyExpr
    | FSharpExprPatterns.NewObject(objType, typeArgs, argExprs) -> 
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.NewRecord(recordType, argExprs) ->  
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.NewAnonRecord(recordType, argExprs) ->  
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.NewTuple(tupleType, argExprs) -> 
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.NewUnionCase(unionType, unionCase, argExprs) -> 
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.Quote(quotedExpr) -> 
        visitExpr f (n + 1) quotedExpr
    | FSharpExprPatterns.FSharpFieldGet(objExprOpt, recordOrClassType, fieldInfo) -> 
        visitObjArg f (n + 1) objExprOpt
    | FSharpExprPatterns.AnonRecordGet(objExpr, recordOrClassType, fieldInfo) -> 
        visitExpr f (n + 1) objExpr
    | FSharpExprPatterns.FSharpFieldSet(objExprOpt, recordOrClassType, fieldInfo, argExpr) -> 
        visitObjArg f (n + 1) objExprOpt; visitExpr f (n + 1) argExpr
    | FSharpExprPatterns.Sequential(firstExpr, secondExpr) -> 
        visitExpr f (n + 1) firstExpr; visitExpr f (n + 1) secondExpr
    | FSharpExprPatterns.TryFinally(bodyExpr, finalizeExpr, dbgTry, dbgFinally) -> 
        visitExpr f (n + 1) bodyExpr; visitExpr f (n + 1) finalizeExpr
    | FSharpExprPatterns.TryWith(bodyExpr, _, _, catchVar, catchExpr, dbgTry, dbgWith) -> 
        visitExpr f (n + 1) bodyExpr; visitExpr f (n + 1) catchExpr
    | FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, tupleExpr) -> 
        visitExpr f (n + 1) tupleExpr
    | FSharpExprPatterns.DecisionTree(decisionExpr, decisionTargets) -> 
        visitExpr f (n + 1) decisionExpr; List.iter (snd >> visitExpr f (n + 1)) decisionTargets
    | FSharpExprPatterns.DecisionTreeSuccess (decisionTargetIdx, decisionTargetExprs) -> 
        visitExprs f (n + 1) decisionTargetExprs
    | FSharpExprPatterns.TypeLambda(genericParam, bodyExpr) -> 
        visitExpr f (n + 1) bodyExpr
    | FSharpExprPatterns.TypeTest(ty, inpExpr) -> 
        visitExpr f (n + 1) inpExpr
    | FSharpExprPatterns.UnionCaseSet(unionExpr, unionType, unionCase, unionCaseField, valueExpr) -> 
        visitExpr f (n + 1) unionExpr; visitExpr f (n + 1) valueExpr
    | FSharpExprPatterns.UnionCaseGet(unionExpr, unionType, unionCase, unionCaseField) -> 
        visitExpr f (n + 1) unionExpr
    | FSharpExprPatterns.UnionCaseTest(unionExpr, unionType, unionCase) -> 
        visitExpr f (n + 1) unionExpr
    | FSharpExprPatterns.UnionCaseTag(unionExpr, unionType) -> 
        visitExpr f (n + 1) unionExpr
    | FSharpExprPatterns.ObjectExpr(objType, baseCallExpr, overrides, interfaceImplementations) -> 
        visitExpr f (n + 1) baseCallExpr
        List.iter (visitObjMember f n) overrides
        List.iter (snd >> List.iter (visitObjMember f n)) interfaceImplementations
    | FSharpExprPatterns.TraitCall(sourceTypes, traitName, typeArgs, typeInstantiation, argTypes, argExprs) ->
        // todo
        visitExprs f (n + 1) argExprs
    | FSharpExprPatterns.ValueSet(valToSet, valueExpr) -> 
        visitExpr f (n + 1) valueExpr
    | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, dbg) -> 
        visitExpr f (n + 1) guardExpr; visitExpr f (n + 1) bodyExpr
    | FSharpExprPatterns.BaseValue baseType -> ()
    | FSharpExprPatterns.DefaultValue defaultType -> ()
    | FSharpExprPatterns.ThisValue thisType -> ()
    | FSharpExprPatterns.Const(constValueObj, constType) -> ()
    | FSharpExprPatterns.Value(valueToGet) -> ()
    | _ -> failwith (sprintf "unrecognized %+A" e)

and visitExprs f n exprs = 
    List.iter (visitExpr f n) exprs

and visitObjArg f n objOpt = 
    Option.iter (visitExpr f n) objOpt

and visitObjMember f n memb = 
    visitExpr f n memb.Body