panel_card <- function(icon_str, ...) {
    
    div(
        class = "col-sm-4",
        div(
            class = "panel panel-default",
            div(
                class = "panel-heading",
                style = "text-align: center; padding: 20px;",
                icon(icon_str, class = "fa-4x fa-boarder")
            ),
            div(
                class = "panel-body",
                style = "padding: 20px;",
                ...
            )
        )
    )
}