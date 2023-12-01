module TypstSyntax

module private Utils =
    let indent (text: string) =
        let numIndent = 4
        let spaces = " " |> String.replicate numIndent
        text.Split [| '\n' |] |> Seq.map (sprintf "%s%s" spaces) |> String.concat "\n"

[<RequireQualifiedAccess>]
type Context =
    | Markup
    | Code
    | Math

    member this.IsRequiringHash =
        match this with
        | Code -> false
        | Markup
        | Math -> true

[<Interface>]
type IComposable =
    abstract member Compose: Context -> string

[<Interface>]
type IExpr =
    inherit IComposable

[<Interface>]
type IMath =
    inherit IExpr
    abstract member ComposeMath: string

    static member compose (context: Context) (composeMath: string) =
        match context with
        | Context.Math -> composeMath
        | _ -> $"$%s{composeMath}$"

let compose (context: Context) (v: IComposable) = v.Compose context

type Markup =
    | Markup of exprs: IExpr list

    member this.Exprs =
        match this with
        | Markup exprs -> exprs

    interface IComposable with
        member this.Compose context =
            this.Exprs |> Seq.map (compose context) |> String.concat ""

    member this.Compose context = this |> compose context

//

/// An identifier: `left`.
type Ident =
    | Ident of name: string

    member this.Name =
        match this with
        | Ident name -> name

    interface IExpr with
        member this.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}%s{this.Name}"

//

type Named =
    { Name: Ident
      Expr: IExpr }

    member this.Compose =
        let name = this.Name.Name
        let expr = this.Expr |> compose Context.Code
        $"%s{name}: %s{expr}"

type Keyed =
    { Key: IExpr
      Expr: IExpr }

    member this.Compose =
        let key = this.Key |> compose Context.Code
        let expr = this.Expr |> compose Context.Code
        $"%s{key}: %s{expr}"

//

type Spread =
    { Name: Ident option
      Expr: IExpr option }

    member this.Compose =
        let name =
            match this.Name with
            | Some n -> $"%s{n.Name} "
            | None -> ""

        let expr =
            match this.Expr with
            | Some e -> e.Compose Context.Code
            | None -> ""

        $"%s{name}..%s{expr}"

type Underscore =
    | Underscore

    member __.Compose = "_"

type DestructuringKind =
    | Normal of IExpr
    | Sink of Spread
    | Named of Named
    | Placeholder of Underscore

    member this.Compose =
        match this with
        | Normal e -> e.Compose Context.Code
        | Sink s -> s.Compose
        | Named n -> n.Compose
        | Placeholder p -> p.Compose

type Destructuring =
    { Bindings: DestructuringKind list
      Idents: Ident list }

    member this.Compose =
        let bindings = this.Bindings |> Seq.map _.Compose |> String.concat ", "
        let idents = this.Idents |> Seq.map _.Name |> String.concat ", "

        $"%s{bindings} %s{idents}"

type Pattern =
    | Normal of IExpr
    | Placeholder of Underscore
    | Destructuring of Destructuring

    member this.Compose =
        match this with
        | Normal e -> e.Compose Context.Code
        | Placeholder _ -> "_"
        | Destructuring d -> d.Bindings |> Seq.map _.Compose |> String.concat ", "

//

type QuoteKind =
    | SingleQuote
    | DoubleQuote

    member this.AsChar =
        match this with
        | SingleQuote -> '\''
        | DoubleQuote -> '"'

type TypstLabel =
    | TypstLabel of string

    member this.AsString =
        match this with
        | TypstLabel s -> s

//

/// Plain text without markup.
type Text =
    | Text of text: string

    member this.Text_ =
        match this with
        | Text text -> text

    interface IExpr with
        member this.Compose context =
            match context with
            | Context.Markup -> this.Text_
            | Context.Math when this.Text_.Length = 1 -> this.Text_
            | _ ->
                let escaped =
                    this.Text_
                    |> Seq.map (function
                        | '"' -> "\\\""
                        | '\\' -> "\\\\"
                        | c -> string c)
                    |> String.concat ""

                let hash = if context.IsRequiringHash then "#" else ""
                $"%s{hash}text(\"%s{escaped}\")"

/// Whitespace in markup or math. Has at most one newline in markup, as more
/// indicate a paragraph break.
type Space =
    | Space

    interface IExpr with
        member __.Compose _ = " "

/// A forced line break: `\`.
type Linebreak =
    | Linebreak

    interface IExpr with
        member __.Compose _ = "\\"

/// A paragraph break, indicated by one or multiple blank lines.
type Parbreak =
    | Parbreak

    interface IExpr with
        member __.Compose _ = "\n\n"

/// An escape sequence: `\#`, `\u{1F5FA}`.
type Escape =
    | SyntaxChar of char
    | Unicode of int

    interface IExpr with
        member this.Compose _ =
            match this with
            | SyntaxChar c -> $"\\%c{c}"
            | Unicode i -> $"\\u{{%x{i}}}"

/// A shorthand for a unicode codepoint. For example, `~` for non-breaking
/// space or `-?` for a soft hyphen.
type Shorthand =
    | Shorthand of string

    interface IExpr with
        member this.Compose _ =
            match this with
            | Shorthand s -> s

/// A smart quote: `'` or `"`.
type SmartQuote =
    | SmartQuote of QuoteKind

    member this.QuoteKind =
        match this with
        | SmartQuote q -> q

    interface IExpr with
        member this.Compose _ = this.QuoteKind.AsChar |> string

/// Strong content: `*Strong*`.
type Strong =
    | Strong of Markup

    interface IExpr with
        member this.Compose context =
            let (Strong m) = this

            match context with
            | Context.Markup ->
                let composed = m.Compose Context.Markup
                $" *%s{composed}* "
            | _ ->
                let composed = m.Compose Context.Code
                let hash = if context.IsRequiringHash then "#" else ""
                $"%s{hash}strong(%s{composed})"

/// Emphasized content: `_Emphasized_`.
type Emph =
    | Emph of Markup

    interface IExpr with
        member this.Compose context =
            let (Emph m) = this

            match context with
            | Context.Markup ->
                let composed = m.Compose Context.Markup
                $" _%s{composed}_ "
            | _ ->
                let composed = m.Compose Context.Code
                let hash = if context.IsRequiringHash then "#" else ""
                $"%s{hash}emph(%s{composed})"

/// Raw text with optional syntax highlighting: `` `...` ``.
type Raw =
    { Body: string
      Lang: string option
      IsBlock: bool }

    interface IExpr with
        member this.Compose context =
            match context with
            | Context.Markup ->
                // Backticks with the least odd number of backticks that is greater than
                // the number of backticks in the text.
                let backticks =
                    let rec maxBackticks' count currentMax chars =
                        match chars with
                        | [] -> currentMax
                        | c :: rest ->
                            let newCount = if c = '`' then count + 1 else 0
                            let newMax = max currentMax newCount
                            maxBackticks' newCount newMax rest

                    let maxBackticks = maxBackticks' 0 0
                    let textBackticks = this.Body |> Seq.toList |> maxBackticks
                    // The least odd number ≥ textBackticks.
                    let numBackticks = (textBackticks - 1) % 2 * 2 + 1
                    String.replicate numBackticks "`"

                let lang =
                    match this.Lang with
                    | Some l -> $":%s{l}"
                    | None -> ""

                let open_ = backticks + lang
                let close = backticks

                let content =
                    if this.IsBlock then
                        this.Body |> Utils.indent |> sprintf "\n%s\n"
                    else
                        $" %s{this.Body} "

                $"%s{open_} %s{content} %s{close}"
            | _ ->
                let hash = if context.IsRequiringHash then "#" else ""
                $"%s{hash}raw(%s{this.Body})"


/// A hyperlink: `https://typst.org`.
type Link =
    | Link of uri: string

    member this.Uri =
        match this with
        | Link uri -> uri

    interface IExpr with
        member this.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}link(%s{this.Uri})"

/// A label: `<intro>`.
type Label =
    | Label of TypstLabel

    member this.LabelText =
        match this with
        | Label l -> l

    interface IExpr with
        member this.Compose context =
            match context with
            | Context.Markup
            | Context.Code -> $" <%s{this.LabelText.AsString}> "
            | _ -> $"#label(%s{this.LabelText.AsString})"

/// A reference: `@target`, `@target[..]`.
type Ref =
    { Target: TypstLabel
      Supplement: string option }

    static member mk(target: TypstLabel, ?supplement: string) =
        { Target = target
          Supplement = supplement }

    interface IExpr with
        member this.Compose context =
            match context with
            | Context.Markup ->
                let target = this.Target.AsString |> sprintf "@%s"

                let supplement =
                    match this.Supplement with
                    | None -> ""
                    | Some supplement -> $"[%s{supplement}]"

                $"%s{target}%s{supplement}"
            | _ ->
                let target = this.Target.AsString |> sprintf "@%s"

                let supplement =
                    match this.Supplement with
                    | None -> ""
                    | Some supplement -> $", supplement: %s{supplement}"

                let hash = if context.IsRequiringHash then "#" else ""

                $"%s{hash}ref(%s{target}%s{supplement})"

/// A section heading: `= Introduction`.
type Heading =
    { Level: int
      Body: Markup }

    static member mk level body = { Level = level; Body = Markup body }

    interface IExpr with
        member this.Compose context =
            let body = this.Body.Compose context

            match context with
            | Context.Markup ->
                let level = "=" |> String.replicate this.Level
                $"%s{level} %s{body}\n"
            | _ ->
                let level = this.Level
                let hash = if context.IsRequiringHash then "#" else ""
                $"%s{hash}heading(%d{level}, %s{body})\n"

/// An item in a bullet list: `- ...`.
type ListItem =
    | ListItem of Markup

    interface IExpr with
        member this.Compose context =
            let (ListItem m) = this
            let item = m.Compose context

            match context with
            | Context.Markup -> $"- %s{item}\n"
            | _ ->
                let hash = if context.IsRequiringHash then "#" else ""
                $"%s{hash}list.item(%s{item})\n"

/// An item in an enumeration (numbered list): `+ ...` or `1. ...`.
type EnumItem =
    { Number: int option
      Body: Markup }

    static member mk(body, ?number) = { Number = number; Body = body }

    static member indexed number body = EnumItem.mk (body, number)

    member this.Compose context =
        match context with
        | Context.Markup ->
            let number =
                match this.Number with
                | Some n -> $"%d{n}."
                | None -> "+"

            let body = this.Body.Compose context
            $"%s{number} %s{body}\n"
        | _ ->
            let number =
                match this.Number with
                | Some n -> $"%d{n}"
                | None -> ""

            let body = this.Body.Compose Context.Code
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}list.item(%s{number}, %s{body})\n"

/// An item in a term list: `/ Term: Details`.
type TermItem =
    { Term: Markup
      Description: Markup }

    static member mk term description =
        { Term = term
          Description = description }

    interface IExpr with
        member this.Compose context =
            let term = this.Term.Compose Context.Markup

            let description =
                this.Description.Compose Context.Markup |> Utils.indent |> sprintf "\n%s\n"

            $"/ %s{term}: %s{description}"

/// The contents of a mathematical equation: `x^2 + 1`.
type Math =
    | Math of exprs: IExpr list

    member this.Exprs =
        match this with
        | Math exprs -> exprs

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            this.Exprs |> Seq.map (compose Context.Math) |> String.concat " "

/// A mathematical equation: `$x$`, `$ x^2 $`.
type Equation =
    { Body: Math
      IsBlock: bool }

    static member inline_(body: Math) = { Body = body; IsBlock = false }

    static member block(body: Math) = { Body = body; IsBlock = true }

    interface IExpr with
        member this.Compose context =
            let body = this.Body |> compose Context.Math

            match context with
            | Context.Markup
            | Context.Code ->
                let body = this.Body |> compose Context.Math

                if this.IsBlock then
                    body |> Utils.indent |> sprintf "$\n%s\n$"
                else
                    $"$%s{body}$"
            | Context.Math ->
                let block = if this.IsBlock then ", block: true" else ""
                $"#math.equation($%s{body}$%s{block})"

/// An identifier in math: `pi`.
type MathIdent =
    | MathIdent of name: string

    member this.Name =
        match this with
        | MathIdent name -> name

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath = this.Name


/// An alignment point in math: `&`.
type MathAlignPoint =
    | MathAlignPoint

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member __.ComposeMath = "&"

/// Matched delimiters in math: `[x + y]`.
type MathDelimited =
    { Open: IExpr
      Body: Math
      Close: IExpr }

    static member mk(open_: IExpr, body: Math, close: IExpr) =
        { Open = open_
          Body = body
          Close = close }

    static member mk(open_: string, body: Math, close: string) =
        MathDelimited.mk (open_ = MathIdent open_, body = body, close = MathIdent close)

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            [ this.Open; this.Body; this.Close ]
            |> Seq.map (compose Context.Math)
            |> String.concat ""

/// Grouped math primes.
type MathPrimes =
    { Count: int }

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            let primes = "'" |> String.replicate this.Count
            primes

/// A base with optional attachments in math: `a_1^2`.
type MathAttach =
    { Base: IExpr
      Bottom: IExpr option
      Top: IExpr option
      Primes: MathPrimes option }

    static member mk(base_: IExpr, ?bottom: IExpr, ?top: IExpr, ?primes: MathPrimes) =
        { Base = base_
          Bottom = bottom
          Top = top
          Primes = primes }

    static member (^%)(this: MathAttach, top: IExpr) = { this with MathAttach.Top = Some top }

    static member (-%)(this: MathAttach, bottom: IExpr) =
        { this with
            MathAttach.Bottom = Some bottom }

    member this.WithPrime =
        { this with
            Primes =
                match this.Primes with
                | Some p ->
                    Some
                        { p with
                            MathPrimes.Count = p.Count + 1 }
                | None -> Some { MathPrimes.Count = 1 } }

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            let base_ = this.Base |> compose Context.Math

            let bottom =
                match this.Bottom with
                | Some b -> $"_%s{b.Compose Context.Math}"
                | None -> ""

            let top =
                match this.Top with
                | Some t -> $"^%s{t.Compose Context.Math}"
                | None -> ""

            let primes =
                match this.Primes with
                | Some p -> p |> compose Context.Math
                | None -> ""

            $"%s{base_}%s{bottom}%s{top}%s{primes}"

/// A fraction in math: `x/2`.
type MathFrac =
    { Numerator: IExpr
      Denominator: IExpr }

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            [ this.Numerator; this.Denominator ]
            |> Seq.map (compose Context.Math)
            |> String.concat "/"

[<RequireQualifiedAccess>]
type RootKind =
    | Square
    | Cube
    | Fourth

    member this.AsString =
        match this with
        | Square -> "√"
        | Cube -> "∛"
        | Fourth -> "∜"

/// A root in math: `√x`, `∛x` or `∜x`.
type MathRoot =
    { Kind: RootKind
      Radicand: IExpr }

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            let root = this.Kind.AsString
            let radicand = this.Radicand.Compose Context.Math
            $"%s{root}%s{radicand}"

/// The `none` literal.
type NoneLiteral =
    | NoneLiteral

    interface IExpr with
        member __.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}none"

/// The `auto` literal.
type Auto =
    | Auto

    interface IExpr with
        member __.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}auto"

/// A boolean: `true`, `false`.
type Bool =
    { Value: bool }

    interface IExpr with
        member this.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""

            match this.Value with
            | true -> $"%s{hash}true"
            | false -> $"%s{hash}false"

/// An integer: `120`.
type Int =
    { Value: int64 }

    interface IExpr with
        member this.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}%d{this.Value}"

/// A floating-point number: `1.2`, `10e-4`.
type Float =
    { Value: float }

    interface IExpr with
        member this.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}%f{this.Value}"

type NumericUnit =
    | Pt
    | Mm
    | Cm
    | In
    | Rad
    | Deg
    | Em
    | Fr
    | Percent

    member this.AsString =
        match this with
        | Pt -> "pt"
        | Mm -> "mm"
        | Cm -> "cm"
        | In -> "in"
        | Rad -> "rad"
        | Deg -> "deg"
        | Em -> "em"
        | Fr -> "fr"
        | Percent -> "%"

/// A numeric value with a unit: `12pt`, `3cm`, `2em`, `90deg`, `50%`.
type Numeric =
    { Value: float
      Unit: NumericUnit }

    interface IExpr with
        member this.Compose context =
            let value = this.Value
            let unit = this.Unit.AsString
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}%f{value}%s{unit}"

/// A quoted string: `"..."`.
type Str =
    | Str of value: string

    member this.Value =
        match this with
        | Str value -> value

    interface IExpr with
        member this.Compose _ = $"\"%s{this.Value}\""

type Code = { Exprs: IExpr list }

/// A code block: `{ let x = 1; x + 2 }`.
type CodeBlock =
    { Body: Code }

    interface IExpr with
        member this.Compose context =
            let body =
                this.Body.Exprs
                |> Seq.map (compose Context.Code)
                |> String.concat "\n"
                |> Utils.indent

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}{{\n%s{body}\n}}"

/// A content block: `[*Hi* there!]`.
type ContentBlock =
    { Body: Markup }

    interface IExpr with
        member this.Compose context =
            let body = this.Body.Compose Context.Markup |> Utils.indent |> sprintf "\n%s\n"
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}[%s{body}]"

/// A grouped expression: `(1 + 2)`.
type Parenthesized =
    | Parenthesized of IExpr

    static member mk(expr: IExpr) = Parenthesized expr

    member this.Expr =
        match this with
        | Parenthesized e -> e

    interface IExpr with
        member this.Compose context =
            let expr = this.Expr.Compose context
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}(%s{expr})"

type ArrayItem =
    | Pos of IExpr
    | Spread of IExpr

    member this.Compose =
        match this with
        | Pos e -> e.Compose Context.Code
        | Spread e -> $"..%s{e.Compose Context.Code}"

/// An array: `(1, "hi", 12cm)`.
type Array =
    { Items: ArrayItem list }

    interface IExpr with
        member this.Compose context =
            let items = this.Items |> Seq.map _.Compose |> String.concat ", "

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}(%s{items})"

type DictItem =
    | Named of Named
    | Keyed of Keyed
    | Spread of IExpr

    member this.Compose =
        match this with
        | Named n -> n.Compose
        | Keyed k -> k.Compose
        | Spread e -> $"..%s{e.Compose Context.Code}"

/// A dictionary: `(thickness: 3pt, pattern: dashed)`.
type Dict =
    { Items: DictItem list }

    interface IExpr with
        member this.Compose context =
            let items = this.Items |> Seq.map _.Compose |> String.concat ", "

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}(%s{items})"

type UnOp =
    | Pos
    | Neg
    | Not

    member this.AsString =
        match this with
        | Pos -> "+"
        | Neg -> "-"
        | Not -> "!"

/// A unary operation: `-x`.
type Unary =
    { Op: UnOp
      Expr: IExpr }

    member this.Compose(context: Context) =
        let op = this.Op.AsString
        let expr = this.Expr.Compose Context.Code

        if context.IsRequiringHash then
            $"%s{op}%s{expr}"
        else
            $"#(%s{op}%s{expr})"

type BinOp =
    | Add
    | Sub
    | Mul
    | Div
    | And
    | Or
    | Eq
    | Neq
    | Lt
    | Leq
    | Gt
    | Geq
    | Assign
    | In
    | NotIn
    | AddAssign
    | SubAssign
    | MulAssign
    | DivAssign

    member this.AsString =
        match this with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | And -> "and"
        | Or -> "or"
        | Eq -> "=="
        | Neq -> "!="
        | Lt -> "<"
        | Leq -> "<="
        | Gt -> ">"
        | Geq -> ">="
        | Assign -> "="
        | In -> "in"
        | NotIn -> "not in"
        | AddAssign -> "+="
        | SubAssign -> "-="
        | MulAssign -> "*="
        | DivAssign -> "/="

/// A binary operation: `a + b`.
type Binary =
    { Op: BinOp
      Lhs: IExpr
      Rhs: IExpr }

    interface IExpr with
        member this.Compose(context: Context) =
            let lhs = this.Lhs.Compose Context.Code
            let op = this.Op.AsString
            let rhs = this.Rhs.Compose Context.Code

            if context.IsRequiringHash then
                $"%s{lhs} %s{op} %s{rhs}"
            else
                $"#(%s{lhs} %s{op} %s{rhs})"

/// A field access: `properties.age`.
type FieldAccess =
    { Target: IExpr
      Field: Ident }

    member this.Compose(context: Context) =
        let target = this.Target.Compose Context.Code
        let field = this.Field.Name
        let hash = if context.IsRequiringHash then "#" else ""
        $"%s{hash}%s{target}.%s{field}"


type Arg =
    | Pos of IExpr
    | Named of Named
    | Spread of IExpr

    static member pos = Pos

    static member named(name: Ident) =
        fun expr -> Named { Named.Expr = expr; Named.Name = name }

    static member named(name: string) = Arg.named (name = Ident name)

    static member spread = Spread

    member this.Compose(context: Context) =
        match this with
        | Pos e -> e.Compose context
        | Named n -> n.Compose
        | Spread e -> $"..%s{e.Compose context}"

type Args =
    { Items: Arg list
      HasTrailingComma: bool }

    static member mk(items: Arg seq, ?hasTrailingComma: bool) =
        { Items = items |> Seq.toList
          HasTrailingComma = hasTrailingComma |> Option.defaultValue false }

    member this.Compose(context: Context) =
        let items = this.Items |> Seq.map _.Compose(context) |> String.concat ", "

        let trailingComma = if this.HasTrailingComma then "," else ""

        $"%s{items}%s{trailingComma}"

/// An invocation of a function or method: `f(x, y)`.
type FuncCall =
    { Callee: IExpr
      Args: Args }

    static member mk(callee: IExpr) =
        fun (args: Args) -> { Callee = callee; Args = args }

    static member mk(callee: string) = FuncCall.mk (callee = Ident callee)

    static member call (args: Args) (callee: IExpr) = FuncCall.mk callee args

    static member (<&)(this: FuncCall, expr: IExpr) =
        { this with
            FuncCall.Args.Items = this.Args.Items @ [ Arg.pos expr ] }

    static member (<&)(this: FuncCall, arg: Arg) =
        { this with
            FuncCall.Args.Items = this.Args.Items @ [ arg ] }

    static member (<&&)(this: FuncCall, args: Arg seq) =
        { this with
            FuncCall.Args.Items = this.Args.Items @ (args |> Seq.toList) }

    static member (<..)(this: FuncCall, expr: IExpr) =
        { this with
            FuncCall.Args.Items = this.Args.Items @ [ Arg.spread expr ] }

    interface IExpr with
        member this.Compose context =
            let callee = this.Callee.Compose Context.Code
            let args = this.Args.Compose Context.Code
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}%s{callee}(%s{args})"

/// Extra type for deparsing: method call of `math` in math context.
type MathFuncCall =
    { Callee: IExpr
      Args: Args }

    static member mk(callee: IExpr) =
        fun (args: Args) -> { Callee = callee; Args = args }

    static member mk(callee: string) =
        MathFuncCall.mk (callee = MathIdent callee)

    static member call (args: Args) (callee: IExpr) = MathFuncCall.mk callee args

    static member (<&)(this: MathFuncCall, expr: IExpr) =
        { this with
            MathFuncCall.Args.Items = this.Args.Items @ [ Arg.pos expr ] }

    static member (<&)(this: MathFuncCall, arg: Arg) =
        { this with
            MathFuncCall.Args.Items = this.Args.Items @ [ arg ] }

    static member (<&&)(this: MathFuncCall, args: Arg seq) =
        { this with
            MathFuncCall.Args.Items = this.Args.Items @ (args |> Seq.toList) }

    static member (<..)(this: MathFuncCall, expr: IExpr) =
        { this with
            MathFuncCall.Args.Items = this.Args.Items @ [ Arg.spread expr ] }

    interface IMath with
        member this.Compose context =
            this :> IMath |> _.ComposeMath |> IMath.compose context

        member this.ComposeMath =
            let callee = this.Callee.Compose Context.Math
            let args = this.Args.Compose Context.Math
            $"%s{callee}(%s{args})"

type Param =
    | Pos of Pattern
    | Named of Named
    | Sink of Spread

    member this.Compose =
        match this with
        | Pos p -> p.Compose
        | Named n -> n.Compose
        | Sink s -> s.Compose

type Params = { Children: Param list }

/// A closure: `(x, y) => z`.
type Closure =
    { Name: Ident option
      Params: Params
      Body: IExpr }

    interface IExpr with
        member this.Compose context =
            let name =
                match this.Name with
                | Some n -> $"%s{n.Name} "
                | None -> ""

            let params_ = this.Params.Children |> Seq.map _.Compose |> String.concat ", "

            let body = this.Body.Compose Context.Code

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}(%s{name}%s{params_}) => %s{body}"

type LetBindingKind =
    | Normal of Pattern
    | Closure of Ident

/// A let binding: `let x = 1`.
type LetBinding =
    { Kind: LetBindingKind
      Init: IExpr option }

    interface IExpr with
        member this.Compose context =
            let kind =
                match this.Kind with
                | Normal p -> p.Compose
                | Closure i -> i.Name

            let init =
                match this.Init with
                | Some i -> i.Compose Context.Code
                | None -> ""

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}let %s{kind} = %s{init}\n"

//// A destructuring assignment: `(x, y) = (1, 2)`.
type DestructAssignment =
    { Pattern: Pattern
      Value: IExpr }

    interface IExpr with
        member this.Compose context =
            let pattern = this.Pattern.Compose
            let value = this.Value.Compose Context.Code
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}(%s{pattern}) = %s{value}\n"

/// A set rule: `set text(...)`.
type SetRule =
    { Target: IExpr
      Args: Args
      Condition: IExpr option }

    static member mk(target: IExpr) =
        { Target = target
          Args = { Items = []; HasTrailingComma = false }
          Condition = None }

    static member mk(target: string) = SetRule.mk (target = Ident target)

    static member (<&)(this: SetRule, expr: IExpr) =
        { this with
            SetRule.Args.Items = this.Args.Items @ [ Arg.pos expr ] }

    static member (<&)(this: SetRule, arg: Arg) =
        { this with
            SetRule.Args.Items = this.Args.Items @ [ arg ] }

    static member (<&&)(this: SetRule, args: Arg seq) =
        { this with
            SetRule.Args.Items = this.Args.Items @ (args |> Seq.toList) }

    member this.WithTrailingComma =
        { this with
            SetRule.Args.HasTrailingComma = true }

    member this.WithCondition(condition: IExpr) =
        { this with
            SetRule.Condition = Some condition }

    interface IExpr with
        member this.Compose context =
            let target = this.Target.Compose Context.Code
            let args = this.Args.Compose Context.Code

            let condition =
                match this.Condition with
                | Some c -> $" if %s{c.Compose Context.Code}"
                | None -> ""

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}set %s{target}(%s{args})%s{condition}\n"

/// A show rule: `show heading: it => emph(it.body)`.
type ShowRule =
    { Selector: IExpr option
      Transform: IExpr }

    interface IExpr with
        member this.Compose context =
            let selector =
                match this.Selector with
                | Some s -> $"%s{s.Compose Context.Code}: "
                | None -> ""

            let transform = this.Transform.Compose Context.Code

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}show %s{selector}%s{transform}\n"

/// An if-else conditional: `if x { y } else { z }`.
type Conditional =
    { Condition: IExpr
      IfBody: IExpr
      ElseBody: IExpr option }

    interface IExpr with
        member this.Compose context =
            let condition = this.Condition.Compose Context.Code
            let ifBody = this.IfBody.Compose Context.Code |> Utils.indent |> sprintf "{\n%s\n}"

            let elseBody =
                match this.ElseBody with
                | Some e -> e.Compose Context.Code |> Utils.indent |> sprintf " else {\n%s\n}"
                | None -> ""

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}if %s{condition} %s{ifBody}%s{elseBody}\n"

/// A while loop: `while x { y }`.
type WhileLoop =
    { Condition: IExpr
      Body: IExpr }

    interface IExpr with
        member this.Compose context =
            let condition = this.Condition.Compose Context.Code
            let body = this.Body.Compose Context.Code |> Utils.indent |> sprintf "{\n%s\n}"

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}while %s{condition} %s{body}\n"

/// A for loop: `for x in y { z }`.
type ForLoop =
    { Pattern: Pattern
      Iter: IExpr
      Body: IExpr }

    interface IExpr with
        member this.Compose context =
            let pattern = this.Pattern.Compose
            let iter = this.Iter.Compose Context.Code
            let body = this.Body.Compose Context.Code |> Utils.indent |> sprintf "{\n%s\n}"
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}for %s{pattern} in %s{iter} %s{body}\n"

type RenamedImportItem =
    { OrdinalName: Ident
      NewName: Ident }

    member this.Compose = sprintf "%s as %s" this.OrdinalName.Name this.NewName.Name

type ImportItem =
    | Simple of Ident
    | Renamed of RenamedImportItem

    member this.Compose =
        match this with
        | Simple i -> i.Name
        | Renamed r -> r.Compose

type ImportItems = ImportItem list

type Imports =
    | Wildcard
    | Items of ImportItems

    member this.Compose =
        match this with
        | Wildcard -> "*"
        | Items i -> i |> Seq.map _.Compose |> String.concat ", "

/// A module import: `import "utils.typ": a, b, c`.
type ModuleImport =
    { Source: IExpr
      Imports: Imports option
      NewName: Ident option }

    interface IExpr with
        member this.Compose context =
            let source = this.Source.Compose Context.Code

            let imports =
                match this.Imports with
                | Some i -> $": %s{i.Compose}"
                | None -> ""

            let newName =
                match this.NewName with
                | Some n -> $" as %s{n.Name}"
                | None -> ""

            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}import %s{source}%s{imports}%s{newName}\n"

/// A module include: `include "chapter1.typ"`.
type ModuleInclude =
    { Source: IExpr }

    interface IExpr with
        member this.Compose context =
            let source = this.Source.Compose Context.Code
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}include %s{source}\n"

/// A break from a loop: `break`.
type LoopBreak =
    | LoopBreak

    interface IExpr with
        member __.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}break"

/// A continue in a loop: `continue`.
type LoopContinue =
    | LoopContinue

    interface IExpr with
        member __.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""
            $"%s{hash}continue"

/// A return from a function: `return`, `return x + 1`.
type FuncReturn =
    { Body: IExpr option }

    interface IExpr with
        member this.Compose context =
            let hash = if context.IsRequiringHash then "#" else ""

            let ret =
                match this.Body with
                | Some b -> $"return %s{b.Compose Context.Code}"
                | None -> "return"

            $"%s{hash}%s{ret}"

//

module Script =

    let tt = Text

    let sp = Space

    let str = Str

    let strong = Strong << Markup

    let emph = Emph << Markup

    let escape = SyntaxChar

    let unicode = Unicode

    type SetRuleBuilder() =
        static member (?)(__: SetRuleBuilder, target: string) = SetRule.mk target

    let setRule = SetRuleBuilder()

    let ii = Ident

    type MathIdentBuilder() =
        static member (?)(__: MathIdentBuilder, name: string) = MathIdent name

    let mi = MathIdentBuilder()

    let heading = Heading.mk

    type FuncBuilder() =
        static member (?)(__: FuncBuilder, callee: string) =
            FuncCall.mk (callee = Ident callee) (Args.mk [])

    let func = FuncBuilder()

    type MathFuncBuilder() =
        static member (?)(__: MathFuncBuilder, callee: string) =
            MathFuncCall.mk (callee = MathIdent callee) (Args.mk [])

    let math = MathFuncBuilder()

    let (^%) (base_: IMath) (top: IExpr) = MathAttach.mk base_ ^% top

    let (-%) (base_: IExpr) (bot: IExpr) = MathAttach.mk base_ -% bot

    let inline (!**=) (expr: IExpr) = Arg.pos expr

    let inline ( *=* ) (name: string) (expr: IExpr) = Arg.named name expr

    let named name expr = Arg.named (name = Ident name) expr

    let paren body = MathDelimited.mk ("(", Math body, ")")

    let inline_ body = Equation.inline_ (Math body)

    let block body = Equation.block (Math body)
