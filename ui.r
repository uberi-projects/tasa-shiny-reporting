## ui.R

# Load packages ---------------------------
library(shiny)
library(rmarkdown)

# Define ui ---------------------------
ui <- fluidPage(
    includeCSS("www/modern-normalize.css"),
    includeCSS("www/styles.css"),
    img(class = "background-image", src = "images/ocean.jpg", style = "position: absolute"),
    div(class = "top-border"),
    div(class = "header-title", h1("Turneffe Reef-Monitoring Data Reporting Tool")),
    div(img(class = "header-logo", src = "images/TASA_logo_full_color.png", alt = "Logo")),
    div(
        class = "nav-bar",
        a(href = "#Home", "Home"),
        a(href = "#Report", "Create Report"),
        a(href = "#Manual", "Manual"),
        a(href = "#Template", "Data Templates"),
        a(href = "#Source", "Source Code")
    ),
    div(
        class = "content-box",
        p("Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim."),
        p("Sed lectus. Donec mollis hendrerit risus. Phasellus nec sem in justo pellentesque facilisis. Etiam imperdiet imperdiet orci. Nunc nec neque. Phasellus leo dolor, tempus non, auctor et, hendrerit quis, nisi. Curabitur ligula sapien, tincidunt non, euismod vitae, posuere imperdiet, leo. Maecenas malesuada. Praesent congue erat at massa. Sed cursus turpis vitae tortor. Donec posuere vulputate arcu. Phasellus accumsan cursus velit. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Sed aliquam, nisi quis porttitor congue, elit erat euismod orci, ac placerat dolor lectus quis orci. Phasellus consectetuer vestibulum elit. Aenean tellus metus, bibendum sed, posuere ac, mattis non, nunc. Vestibulum fringilla pede sit amet augue. In turpis. Pellentesque posuere. Praesent turpis. Aenean posuere, tortor sed cursus feugiat, nunc augue blandit nunc, eu sollicitudin urna dolor sagittis lacus. Donec elit libero, sodales nec, volutpat a, suscipit non, turpis. Nullam sagittis. Suspendisse pulvinar, augue ac venenatis condimentum, sem libero volutpat nibh, nec pellentesque velit pede quis nunc. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia Curae; Fusce id purus. Ut varius tincidunt libero. Phasellus dolor. Maecenas vestibulum mollis"),
        p("Lorem, ipsum dolor sit amet consectetur adipisicing elit. Vitae nemo ad alias dicta ab laudantium soluta natus veniam quo velit
        quisquam non eligendi reprehenderit architecto accusamus nulla, quam quod vero.")
    )
)
