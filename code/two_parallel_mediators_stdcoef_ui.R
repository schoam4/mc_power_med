list(
  withTags(
    table(style = "width: 350px;", id = "STDpath_table",
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("a1")),  
            td(textInput(inputId = "STa1", label = NULL, value = "0.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("a2")),  
            td(textInput(inputId = "STa2", label = NULL, value = "0.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("b1")),  
            td(textInput(inputId = "STb1", label = NULL, value = "0.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("b2")),  
            td(textInput(inputId = "STb2", label = NULL, value = "0.00"))
          ),
          tr(
            td(style="padding-top:0px;padding-left:10px;width:32px;", label("c'")),  
            td(textInput(inputId = "STcprime", label = NULL, value = "0.00"))
          ),
          tr(
            th(style="padding-top:5px;padding-left:10px;width:40px;"),
            th(style="padding-top:5px;", label("X")),
            th(style="padding-top:5px;",label("M1")),
            th(style="padding-top:5px;",label("M2")),
            th(style="padding-top:5px;",label("Y"))
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
