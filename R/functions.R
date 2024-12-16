# Functions to prepare data ----
get_bbb_data <- function () {
  # Fetch data from cricsheet for all one day cricket matches and bind
  bbb_data <- rbind(
    fetch_cricsheet("bbb", "female", "rhf") %>%
      mutate(
        over = ceiling(ball),
        wickets = if_else(!is.na(wicket_type) &
                            wicket_type != "", 1, 0),
      ),
    
    fetch_cricsheet("bbb", "male", "rlc") %>%
      mutate(
        over = ceiling(ball),
        wickets = if_else(!is.na(wicket_type) &
                            wicket_type != "", 1, 0),
      ),
    
    fetch_cricsheet("bbb", "female", "odms") %>%
      mutate(
        over = ceiling(ball),
        wickets = if_else(!is.na(wicket_type) &
                            wicket_type != "", 1, 0),
      ),
    
    fetch_cricsheet("bbb", "male", "odms") %>%
      mutate(
        over = ceiling(ball),
        wickets = if_else(!is.na(wicket_type) &
                            wicket_type != "", 1, 0),
      ),
    
    fetch_cricsheet("bbb", "female", "odis") %>%
      mutate(
        over = ceiling(ball),
        wickets = if_else(!is.na(wicket_type) &
                            wicket_type != "", 1, 0),
      ),
    
    fetch_cricsheet("bbb", "male", "odis") %>%
      mutate(
        over = ceiling(ball),
        wickets = if_else(!is.na(wicket_type) &
                            wicket_type != "", 1, 0),
      )
  )
  
}

unzip_yaml <- function(yaml_files_download) {

   walk(yaml_files_download, function(yaml_files_download) {
     unzip(yaml_files_download, exdir = "temp_yaml_files/")
     })
}

extract_yaml_info <- function (yaml_files) {
  file_paths <- list.files("temp_yaml_files", pattern = ".yaml")
  
  file_paths <- paste("temp_yaml_files/", file_paths, sep = "")
  
  # Define the function to extract results from a single YAML file
  extract_results_from_yaml <- function(file_path) {
    # Load YAML data
    yaml_data <- yaml::yaml.load_file(file_path)
    
    # Extract file name without extension
    file_name <- tools::file_path_sans_ext(basename(file_path))
    
    # Extract competition information
    
    competition <-
      if (!is.null(yaml_data$info$competition))
        yaml_data$info$competition
    else {
      "ODI"
    }
    
    # Extract who won the match
    winning_team <- if ("outcome" %in% names(yaml_data$info)) {
      if ("winner" %in% names(yaml_data$info$outcome)) {
        yaml_data$info$outcome$winner
      } else {
        NA
      }
    } else {
      NA
    }
    
    # Extract who won the toss
    toss_winner <- if ("toss" %in% names(yaml_data$info)) {
      if ("winner" %in% names(yaml_data$info$toss)) {
        yaml_data$info$toss$winner
      } else {
        NA
      }
    } else {
      NA
    }
    
    # Extract if match subject to Duckworth–Lewis–Stern method
    DLS <- if ("outcome" %in% names(yaml_data$info)) {
      if ("method" %in% names(yaml_data$info$outcome)) {
        yaml_data$info$outcome$method
      } else {
        NA
      }
    } else {
      NA
    }
    
    # Create a data frame with extracted information
    results <-
      data.frame(
        competition,
        winning_team,
        toss_winner,
        DLS = DLS,
        match_id = file_name,
        stringsAsFactors = FALSE
      )
    
    return(results)
  }
  
  # Apply the function to each file path using lapply
  results_list <- lapply(file_paths, extract_results_from_yaml)
  
  # Clear the temporary yaml files to save space for git commits
  unlink("temp_yaml_files", recursive = TRUE)
  
  # Combine the list of data frames into a single data frame
  results_df <- dplyr::bind_rows(results_list) %>%
    mutate(winning_team = str_remove(winning_team, "Winner:"))
  
}

summarise_total_scores <- function(bbb_data, results_df) {
  # Summarise bbb match data by batting/bowling team and innings
  overall_match_data <- bbb_data %>%
    group_by(match_id, innings, batting_team, bowling_team) %>%
    summarise(
      total_runs = sum(runs_off_bat, extras),
      total_wickets_lost = sum(wickets),
      total_overs_faced = max(over)
    )
  
  # Combine the bbb match data summary with the information extraction from yaml files
  overall_match_data <-
    merge(overall_match_data,
          results_df,
          by = "match_id",
          all.x = TRUE)
  
  # Filter out any matches with no results, ties, or that were subject to Duckworth–Lewis–Stern method
  total_scores <- overall_match_data %>%
    filter(winning_team != "no result" &
             !is.na(winning_team) & winning_team != "tie") |>
    filter(is.na(DLS))
  
  # Recode team names for "Lightning/The Blaze" as the team changed name
  total_scores <- total_scores %>%
    mutate(batting_team = str_replace_all(
      batting_team,
      regex("Lightning|The Blaze", ignore_case = TRUE),
      "Lightning/The Blaze"
    )) %>%
    mutate(bowling_team = str_replace_all(
      bowling_team,
      regex("Lightning|The Blaze", ignore_case = TRUE),
      "Lightning/The Blaze"
    )) %>%
    mutate(winning_team = str_replace_all(
      winning_team,
      regex("Lightning|The Blaze", ignore_case = TRUE),
      "Lightning/The Blaze"
    ))
  
}

# Directed Acyclic Graph for causal models ----
create_main_dag <- function(){
  dag <- dagitty("dag {

1st_Innings->Batting_Team
1st_Innings->Fielding_Team
Batting_Team->Fielding_wickets
Batting_Team->Leg_Byes
Batting_Team->1s
Batting_Team->2s
Batting_Team->4s
Batting_Team->6s
Batting_Team->Byes
Batting_Team->Maiden_Overs
Batting_Team->Overs_Seam
Batting_Team->Overs_Spin
Fielding_Team->Fielding_wickets
Fielding_Team->Leg_Byes
Fielding_Team->Byes
Fielding_Team->Overs_Seam
Fielding_Team->Overs_Spin
Fielding_wickets->Total_Wickets_Lost
Leg_Byes->Total_Runs
No_Balls->Fielding_wickets
No_Balls->1s
No_Balls->2s
No_Balls->4s
No_Balls->6s
No_Balls->Total_Overs_Faced
No_Balls->Total_Runs
1s->Total_Runs
2s->Total_Runs
4s->Total_Runs
6s->Total_Runs
Byes->Total_Runs
Maiden_Overs->Total_Overs_Faced
Match->Batting_Team
Match->Fielding_Team
Match->Toss_outcome
Overs_Seam->Leg_Byes
Overs_Seam->No_Balls
Overs_Seam->1s
Overs_Seam->2s
Overs_Seam->4s
Overs_Seam->6s
Overs_Seam->Byes
Overs_Seam->Maiden_Overs
Overs_Seam->Total_Overs_Faced
Overs_Seam->Total_Wickets_Lost
Overs_Seam->Wides
Overs_Spin->Leg_Byes
Overs_Spin->No_Balls
Overs_Spin->1s
Overs_Spin->2s
Overs_Spin->4s
Overs_Spin->6s
Overs_Spin->Byes
Overs_Spin->Maiden_Overs
Overs_Spin->Total_Overs_Faced
Overs_Spin->Total_Wickets_Lost
Overs_Spin->Wides
Toss_outcome->1st_Innings
Total_Overs_Faced->Total_Runs
Total_Runs->Win_Lose
Total_Wickets_Lost->Total_Overs_Faced
Wides->Total_Overs_Faced
Wides->Total_Runs
Wides->Total_Wickets_Lost
}

")
}

add_exposure_outcome <- function(dag,exposure,outcome) {
  exposures(dag) <- exposure
  outcomes(dag) <- outcome
  
  return(dag)
}

# What is the effect of batting in the first innings compared to the second innings on total runs? ----
plot_innings_runs_dag <- function(dag) {
  dag |>
    tidy_dagitty(layout = "time_ordered") |>
    dag_adjustment_sets() |>
    filter(set == "{Match}") |>
    node_status() |>
    mutate(label = case_when(
      is.na(status) == TRUE ~ adjusted,
      .default = status
    )) |>
    ggplot(aes(x=x, y=y, xend=xend, yend=yend, color=label)) +
    geom_dag_point() +
    geom_dag_edges_diagonal(edge_alpha = 0.5) +
    geom_dag_label(size = 2.5) +
    scale_color_manual(breaks = c("adjusted", "exposure", "outcome", "unadjusted"), 
                       values = c("#D55E00", "#009E73", "#56B4E9", NA)) +
    theme_dag()
  
}

sample_innings_runs <- function(data) {
  
  set.seed(1988)
  
  first_innings_sample <- data |>
    group_by(match_id) |>
    arrange(innings) |>
    mutate(
      total_runs_batting = case_when(
        innings == 1 ~ lead(total_runs),
        .default = total_runs
      )
    ) |>
    select(match_id, innings, batting_team, bowling_team, total_runs, total_runs_batting) |>
    filter(innings == 1) |>
    ungroup() |>
    slice_sample(prop = 0.1) |>
    pivot_longer(5:6,
                 names_to = "x",
                 values_to = "total_runs") |>
    pivot_longer(3:4,
                 names_to = "batting",
                 values_to = "team") |>
    filter(x == "total_runs" & batting == "batting_team" |
             x == "total_runs_batting" & batting == "bowling_team") |>
    select(match_id, team, batting, total_runs)  |>
    mutate(match_id = as.factor(match_id)) 
  
}

model_innings_runs <- function(sample) {
  model_first_innings_sample <- glmer(total_runs ~ batting + (1|match_id) + (1|team),
                        family = "poisson",
                        data = sample)
}