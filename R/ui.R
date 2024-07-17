ui <- function() {
  bslib <- page_fillable(
    includeCSS("inst/www/styles.css"),
    tags$script(
      HTML(
        "$(document).on('shown.bs.modal','#shiny-modal', function () {
         Shiny.setInputValue(id = 'modal_visible', value = true);
        });
       $(document).on('hidden.bs.modal','#shiny-modal', function () {
         Shiny.setInputValue(id = 'modal_visible', value = false);
       });"
      )
    ),
    tags$div(
      style = "width: 800px; margin: 0 auto;",
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        tags$h1("Hex logo quiz"),
        tags$img(src = "www/logo.png", height = "100px")
      )
    ),
    tags$div(
      style = "width: 600px; margin: 0 auto; text-align: center;",
      h3("Guess the package maintainer from the logo!"),
      p("Defined by the 'cre' role in the package DESCRIPTION file")
    ),
    tags$div(
      style = "width: 600px; margin: 0 auto;",
      card(
        layout_column_wrap(
          width = NULL,
          style = css(grid_template_columns = "8fr 5fr"),
          uiOutput("o_blurred"),
          layout_column_wrap(
            uiOutput("o_quiz"),
            actionButton("i_submit_answer", "Submit answer")
          )
        )
      )
    )
  )
}
