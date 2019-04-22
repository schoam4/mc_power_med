list(
  withTags(
    table(style = "width: 350px;", id = "STDpath_table",
    tr(
      td(style="padding-top:0px;padding-left:10px;width:32px;", label("a")),  
      td(textInput(inputId = "STa", label = NULL, value = "0.00"))
       ),
    tr(
      td(style="padding-top:0px;padding-left:10px;width:32px;", label("b")),  
      td(textInput(inputId = "STb", label = NULL, value = "0.00"))
    ),
    tr(
      td(style="padding-top:0px;padding-left:10px;width:32px;", label("c'")),  
      td(textInput(inputId = "STcprime", label = NULL, value = "0.00"))
    ),
   tr(
            th(style="padding-top:5px;padding-left:10px;width:32px;"),
            th(style="padding-top:5px;", label("X")),
            th(style="padding-top:5px;",label("M")),
            th(style="padding-top:5px;",label("Y"))
          ),
    tr(
        td(style="padding-top:0px;padding-left:10px;width:32px;", label("Std. Deviation")),  
        td(textInput(inputId = "SDX", label = NULL, value = "1.00")),
        td(textInput(inputId = "SDM", label = NULL, value = "1.00")),
        td(textInput(inputId = "SDY", label = NULL, value = "1.00"))
      )
    )  
  )
)
