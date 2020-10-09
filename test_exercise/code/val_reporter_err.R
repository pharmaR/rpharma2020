report_errors <- function(report){


    for (i in 1:length(report)) {
      for (j in 1:length(report[[i]]$results)) {
        if (attr(report[[i]]$results[[j]], "class")[1] == "expectation_failure") {
          cat(paste(report[[i]]$context,"\n") )
          cat(paste("Test:", j,"\n"))
          
          if (stringr::str_length(as.character(report[[i]]$results[[j]])) < 81) {
            cat(paste("  ", report[[i]]$results[[j]]))
          } 
          else {
            string <- as.character(report[[i]]$results[[j]])
            string <- stringr::str_remove_all(string, "\n")
            string <- stringr::str_remove_all(string, "Error: ")
            cat(paste("  ", trimws(substr(string,1,81), which = "left")), "\n")
            cat(paste("  ", trimws(substr(string,82,(stringr::str_length(as.character(report[[i]]$results[[j]])))), "left")))
          }
          cat("\n \n")
        }
      }
    }

}
report_errors(report)