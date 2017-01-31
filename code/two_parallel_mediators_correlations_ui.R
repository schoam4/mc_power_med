list(
  withTags(
    table(id = "correlations_table", style = "width:450px;",
      tr(
        th(style="padding-top:5px;padding-left:10px;width:40px;"),
        th(style="padding-top:5px;", label("X")),
        th(style="padding-top:5px;",label("M1")),
        th(style="padding-top:5px;",label("M2")),
        th(style="padding-top:5px;",label("Y"))
      ),
      tr(
        td(style="padding-left:10px;width:40px;", label("X")),
        td(style="text-align:center", "1.00"),
        td(),
        td(),
        td()
      ),
      tr(
        td(style="padding-left:10px;width:40px;", label("M1")),
        td(textInput(inputId = "cor21", label = NULL, value = "0.00")),
        td(style="text-align:center", "1.00"),
        td(),
        td()
      ),
      tr(
        td(style="padding-left:10px;width:40px;", label("M2")),  
        td(textInput(inputId = "cor31", label = NULL, value = "0.00")),
        td(textInput(inputId = "cor32", label = NULL, value = "0.00")),
        td(style="text-align:center", "1.00"),
        td()
      ),
      tr(
        td(style="padding-left:10px;width:40px;", label("Y")),  
        td(textInput(inputId = "cor41", label = NULL, value = "0.00")),
        td(textInput(inputId = "cor42", label = NULL, value = "0.00")),
        td(textInput(inputId = "cor43", label = NULL, value = "0.00")),
        td(style="text-align:center;font-size:14px;", "1.00")
      ),
      tr(
        td(style="padding-left:10px;width:40px;", label("Std. Deviation")),  
        td(textInput(inputId = "SDX", label = NULL, value = "1.00")),
        td(textInput(inputId = "SDM1", label = NULL, value = "1.00")),
        td(textInput(inputId = "SDM2", label = NULL, value = "1.00")),
        td(textInput(inputId = "SDY", label = NULL, value = "1.00"))
      )
    )  
  )
)
