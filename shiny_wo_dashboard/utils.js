// this function allows communication between javascript and R. The input$current_id
// value is changed to the id of the element that was clicked.
function get_id(clicked_id) {
     Shiny.setInputValue("current_id", clicked_id, {priority: "event"});
}