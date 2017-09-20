##DETECT KEYBOARD ACTIONS
##key being held down
tags$script('
            $(document).on("keydown", function (e) {
            Shiny.onInputChange("keyd", e.which);
            });
            $(document).on("keyup", function (e) {
            Shiny.onInputChange("keyd", "NO");
            });
            ')