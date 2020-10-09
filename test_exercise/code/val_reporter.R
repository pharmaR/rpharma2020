# A new summary reporter for testthat [not technically, DEMO only]

library(dplyr)
library(stringr)
library(purrr)

#' Extract summary 
#' 
#' Extract a summary from a test report
#' @param test_report An executed test report of class, list
#' @return data.frame
rvh_summary <- function(test_report){
  # Extract requirements from context
  cont <- str_split(test_report$context, ":")[[1]] %>%
    str_trim()
  
  # Count failures
  nfail <- 0
  for (j in 1:length(test_report$results)) {
    if (attr(test_report$results[[j]], "class")[1] == "expectation_failure") nfail <- nfail + 1  
  }
  
  # Number of tests
  total = length(test_report$results)
  
  # Create output
  tibble(File = test_report$file,
         `Requirement ID` = cont[1],
         Requirement = cont[2],
         Test = test_report$test,
         Total = total,
         Passed = Total - nfail,
         Failed = nfail)
}

# Summarise all reports
result <- map(report, rvh_summary) %>% 
  bind_rows 

# Overall Test Summary
result_overall <- result %>%
  summarise(Total = sum(Total),
            Passed = sum(Passed),
            Failed = sum(Failed)) %>%
  mutate(`Pass (%)` = round((Passed / Total) * 100, 0))

# By Requirement
result_byreq <- result %>%
  group_by(`Requirement ID`, Requirement) %>%
  summarise(Total = sum(Total),
            Passed = sum(Passed),
            Failed = sum(Failed)) %>%
  mutate(`Pass (%)` = round((Passed / Total) * 100, 0))

# Details
result_detail <- result  %>%
  mutate(`Pass (%)` = round((Passed / Total) * 100, 0))

# Generate Results CSV for exploration
if(!"results" %in% list.files(".")) dir.create("./results")
write.csv(result_byreq, file = "./results/result_byreq.csv", row.names = F)
write.csv(result_detail, file = "./results/result_detail.csv", row.names = F)

