library(DataExplorer)

args <- commandArgs(trailingOnly=TRUE)
data <- read.csv(args[1], stringsAsFactors = FALSE)

# Increase number of categories allowed
report_config <- configure_report(
  plot_bar_args = list(maxcat = 300)
)

# Generate report
output_dir <-  paste0(dirname(args[1]))
file_name <- basename(basename(output_dir))
file_name <- paste0(basename(dirname(output_dir)),"_",file_name, "_eda_report.html")
output_dir <- paste0("./output")

message("Generating report")
create_report(data, 
              output_file = file_name, 
              output_dir = output_dir,
              config = report_config)
message("Report stored in ", output_dir)

# Remove optional filess
unlink("./report_files", recursive = TRUE)

