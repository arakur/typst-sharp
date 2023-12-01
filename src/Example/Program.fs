open TypstSyntax
open TypstSyntax.Script

let markup =
    Markup
        [ setRule?text <&& [ "font" *=* str "Garamond"; "fill" *=* ii "purple" ]
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
