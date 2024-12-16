# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) 
# Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c("tidyverse", 
               "cricketdata", 
               "lme4", 
               "sjPlot",
               "yaml", 
               "here",
               "dagitty",
               "ggdag") # Packages that your targets need for their tasks.
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.2".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source("R/Functions.R")
# tar_source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own:
list(
  # Get bbb and match data, summarise, and timestamp when download occured
  tar_target(name = data,
             command = get_bbb_data()),
  tar_download(
    name = yaml_files_download,
    urls = c(
      "https://cricsheet.org/downloads/rhf.zip",
      "https://cricsheet.org/downloads/rlc.zip",
      "https://cricsheet.org/downloads/odms.zip",
      "https://cricsheet.org/downloads/odis.zip"
    ),
    paths = paste(
      "temp_yaml_files/",
      stringr::str_remove(
        c(
          "https://cricsheet.org/downloads/rhf.zip",
          "https://cricsheet.org/downloads/rlc.zip",
          "https://cricsheet.org/downloads/odms.zip",
          "https://cricsheet.org/downloads/odis.zip"
        ),
        "https://cricsheet.org/downloads/"
      ),
      sep = ""
    ),
    resources = tar_resources(
      url = tar_resources_url(
        seconds_timeout = 1000,
        seconds_interval = 10,
        max_tries = 10
      )
    ),
    method = "curl"
  ),
  tar_target(name = unzipped_yaml_files,
             command = unzip_yaml(yaml_files_download)),
  tar_target(name = extracted_yaml,
             command = extract_yaml_info(unzipped_yaml_files)),
  tar_target(name = total_scores,
             command = summarise_total_scores(data, extracted_yaml)),
  
  # Main DAG
  tar_target(
    name = main_dag,
    command = create_main_dag()
  ),
  
  # Example for sample model
  tar_target(
    name = innings_runs_dag,
    command = add_exposure_outcome(
      main_dag, "1st_Innings", "Total_Runs"
    )
  ),
  
  tar_target(
    name = innings_runs_dag_plot,
    command = plot_innings_runs_dag(innings_runs_dag)
  ),
  
  tar_target(
    name = innings_runs_sample,
    command = sample_innings_runs(total_scores)
  ),
  
  tar_target(
    name = innings_runs_sample_model,
    command = model_innings_runs(innings_runs_sample)
  )
)

