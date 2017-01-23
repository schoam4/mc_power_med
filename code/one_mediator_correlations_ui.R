list(
  withTags(
    table(style = "width: 350px;", id = "correlations_table",
      tr(
        th(style="padding-top:5px;padding-left:10px;width:32px;"),
        th(style="padding-top:5px;", label("X")),
        th(style="padding-top:5px;",label("M")),
        th(style="padding-top:5px;",label("Y"))
      ),
      tr(
        td(style="padding-top:0px;padding-left:10px;width:32px;", label("X")),
        td(style="text-align:center", label("1.00")),
        td(style="padding-top:0px;"),
        td(style="padding-top:0px;")
      ),
      tr(
        td(style="padding-top:0px;padding-left:10px;width:32px;", label("M")),
        td(textInput(inputId = "cor21", label = NULL, value = "0.00")),
        td("1.00"),
        td()
      ),
      tr(
        td(style="padding-top:0px;padding-left:10px;width:32px;", label("Y")),  
        td(textInput(inputId = "cor31", label = NULL, value = "0.00")),
        td(textInput(inputId = "cor32", label = NULL, value = "0.00")),
        td("1.00")
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
