################################################################################
# Determines if a splice junction plot should be created.
#
# The event reactive that triggers creating a splice junction plot is based on
# the Show plots button being clicked and Splice Junction Usage option being
# chosen. The logic is complicated enough to make the eventReactive call
# confusing so it has been extracted to a function.
#
# @param session The current user session.
# @param plot_click The current button click value.
# @param plot_type The type of plot being created, i.e. "splice_junction_usage"
# @return Returns the current Show plots button click value when the plot
#.   should be shown and NULL when it shouldn't.
#
################################################################################

initiate_splice_junction_plot <- function(session, plot_click, plot_type) {
  initialize_last_plot_click(session)

  new_plot_click <- plot_click != session$userData$last_plot_click
  show_splice_junction_usage <- plot_type == "splice_junction_usage"
  if (new_plot_click && show_splice_junction_usage) {
    session$userData$last_plot_click <- plot_click
    plot_click
  } else {
    NULL
  }
}
