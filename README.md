# [WIP] Composing Typst document from .NET



## Example

```fsharp
open TypstSyntax
open TypstSyntax.Script

let markup =
    Markup
        [ setRule?text <& "font" *=* str "Times New Roman" <& "fill" *=* ii "purple"
          setRule?heading <& "numbering" *=* str "1."
          heading 1 [ tt "Our First Section" ]
          tt "A "
          emph [ tt "monad" ]
          tt " is just a monoid "
          block [ paren [ tt "M"; tt ","; mi?mu; tt ","; mi?eta ] ]
          tt " in the monoidal category "
          inline_ [ paren [ str "End"; math?cal <& tt "C"; tt ","; mi?compose; tt ","; tt "I" -% tt "C" ] ]
          tt " of endofunctors, what"
          escape '\''
          tt "s the problem?"
          unicode 0x1F914 ]

System.IO.File.WriteAllText("./sample.typ", markup |> compose Context.Markup)
```

```typst
#set text(font: "Times New Roman", fill: purple)
#set heading(numbering: "1.")
= Our First Section
A  _monad_  is just a monoid $
    (M , mu , eta)
$ in the monoidal category $("End" cal(C) , compose , I_C)$ of endofunctors, what\'s the problem?\u{1f914}
```

[Output](/sample.pdf)
