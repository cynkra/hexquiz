server <- function(input, output, session) {
  selected_question <- reactiveVal(quiz())

  output$o_blurred <- renderUI({
    card_image(
      file = paste0("inst/www/blurred/", selected_question()$package, ".png")
    )
  })

  output$o_quiz <- renderUI({
    radioButtons(
      "i_quiz_answer",
      label = tags$h5("The maintainer of this package is:"),
      choices = selected_question()$authors,
      selected = character(0)
    )
  })

  observe({
    if (is.null(input$i_quiz_answer)) {
      updateActionButton(inputId = "i_submit_answer", disabled = TRUE)
    } else {
      updateActionButton(inputId = "i_submit_answer", disabled = FALSE)
    }
  })

  observeEvent(input$i_submit_answer, {
    if (input$i_quiz_answer == selected_question()$maintainer) {
      prize_text <- "Congratulations! Speak to the team to select a prize!"
      gif_src <- "www/win.gif"
    } else {
      prize_text <- paste0(
        "Unfortunately, not. The correct answer was that ",
        selected_question()$maintainer, " is the maintainer for the package ",
        selected_question()$package, ". Keep studying R!"
      )
      gif_src <- "www/lose.gif"
    }

    showModal(
      modalDialog(
        title = prize_text,
        img(src = gif_src, width = "100%"),
        easyClose = TRUE
      )
    )
  })

  observe({
    input$modal_visible
    if (!is.null(input$modal_visible) && input$modal_visible == FALSE) {
      selected_question(quiz())
    }
  })
}
