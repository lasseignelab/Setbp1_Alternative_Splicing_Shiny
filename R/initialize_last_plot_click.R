################################################################################
# Initialize the plot button click session variable to prevent NULL errors.
#
# As the plot parameter selection form is interacted with, we don't want things
# to change reactively until the show plots button is clicked.  As a result we
# need to keep up with the last click value to compare with the current click
# value to determine if it has changed.  The initial value of session variables
# is NULL which causes problems in if statements.  To resolve the problem, if
# the value of the session variable is NULL then it is replaced with an empty
# string.
#
# @param session The current user session.
#
################################################################################

initialize_last_plot_click <- function(session) {
  session$userData$last_plot_click <-
    first(c(session$userData$last_plot_click, ""))
}
