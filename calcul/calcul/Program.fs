open WebSharper.Html
open WebSharper.JavaScript

type Operator =
    | Oper of (Expr -> Expr)
    | Oper2 of (Expr -> Expr -> Expr)
    member this.Invoke expr1 expr2 =
        match this with
        | Oper(x) -> x expr1
        | Oper2(x) -> x expr1 (Option.get expr2)
and Expr =
    | Exprs of Expr * Operator * Expr
    | Expr of Operator * Expr
    | Num of int
    
let rec eval expr =
    match expr with
    | Num(x) -> x
    | Expr(op, x) -> eval <| op.Invoke x None
    | Exprs(x, op, y) -> eval <| op.Invoke x (Some(y))

let (+.) expr1 expr2 = Num((eval expr1) + (eval expr2))
let (-.) expr1 expr2 = Num((eval expr1) - (eval expr2))
let ( *.) expr1 expr2 = Num((eval expr1) * (eval expr2))
let (/.) expr1 expr2 = Num((eval expr1) / (eval expr2))

let main =
    Div [
        H1 [Text "Calculator"]
        Input [Attr.Type "number"; Attr.Id "num1"; Attr.Placeholder "Enter the first number"]
        Select [
            Attr.Id "operator"
            Option [Text "+"; Attr.Value "+"]
            Option [Text "-"; Attr.Value "-"]
            Option [Text "*"; Attr.Value "*"]
            Option [Text "/"; Attr.Value "/"]
        ]
        Input [Attr.Type "number"; Attr.Id "num2"; Attr.Placeholder "Enter the second number"]
        Button [
            Text "Calculate"
            OnClick (fun _ ->
                let num1 = !!("#num1") |> int
                let num2 = !!("#num2") |> int
                let op = !!("#operator")?value
                let operator =
                    match op with
                    | "+" -> Oper2(+.)
                    | "-" -> Oper2(-.)
                    | "*" -> Oper2( *.)
                    | "/" -> Oper2(/.)
                    | _ -> failwith "Invalid operator"
                let result = Exprs(Num(num1), operator, Num(num2)) |> eval
                JS.Alert(sprintf "Result: %d" result)
            )
        ]
    ]
