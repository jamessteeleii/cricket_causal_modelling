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
  
  # Clear the temporary yaml zip files to save space for git commits
  unlink("temp_yaml_files/rhf.zip", recursive = TRUE)
  unlink("temp_yaml_files/rlc.zip", recursive = TRUE)
  unlink("temp_yaml_files/odms.zip", recursive = TRUE)
  unlink("temp_yaml_files/odis.zip", recursive = TRUE)
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

# Directed Acyclic Graph for causal models -----
create_main_dag <- function(){
  dag <- dagitty("dag {

Fielding_wickets->Total_Wickets_Lost
Leg_Byes->Total_Runs
No_Balls->Fielding_wickets
No_Balls->1s
No_Balls->2s
No_Balls->4s
No_Balls->6s
No_Balls->Total_Balls_Faced
No_Balls->Total_Runs
1s->Total_Runs
2s->Total_Runs
4s->Total_Runs
6s->Total_Runs
Batting->Fielding_wickets
Batting->Leg_Byes
Batting->1s
Batting->2s
Batting->4s
Batting->6s
Batting->Byes
Batting->Dot_Ball
Batting->Seam_Delivery
Batting->Spin_Delivery
Byes->Total_Runs
Dot_Ball->Total_Balls_Faced
Innings->Batting
Match->Opposition
Match->Team
Opposition->Fielding_wickets
Opposition->Leg_Byes
Opposition->No_Balls
Opposition->Byes
Opposition->Seam_Delivery
Opposition->Spin_Delivery
Opposition->Toss_outcome
Seam_Delivery->Leg_Byes
Seam_Delivery->No_Balls
Seam_Delivery->1s
Seam_Delivery->2s
Seam_Delivery->4s
Seam_Delivery->6s
Seam_Delivery->Byes
Seam_Delivery->Dot_Ball
Seam_Delivery->Total_Balls_Faced
Seam_Delivery->Total_Wickets_Lost
Seam_Delivery->Wides
Spin_Delivery->Leg_Byes
Spin_Delivery->No_Balls
Spin_Delivery->1s
Spin_Delivery->2s
Spin_Delivery->4s
Spin_Delivery->6s
Spin_Delivery->Byes
Spin_Delivery->Dot_Ball
Spin_Delivery->Total_Balls_Faced
Spin_Delivery->Total_Wickets_Lost
Spin_Delivery->Wides
Team->Batting
Team->Toss_outcome
Toss_outcome->Batting
Total_Balls_Faced->Total_Runs
Total_Runs->Win_Lose
Total_Wickets_Lost->Total_Balls_Faced
Wides->Total_Balls_Faced
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
    
    # Adds adjustment sets
    dag_adjustment_sets() |>
    
    # Select the adjustment set (note, set exposures/outcome on dagitty and select)
    filter(set == "{Opposition}") |>
    
    # Adds node status (i.e., exposures/outcome and minimal adjustment from adjustment set)
    node_status() |>
    
    # Add any additional adjusted variables (change/add the name conditions)
    mutate(status = case_when(
      (name == "Match" | name == "Team") ~ "adjusted",
      .default = status
    )) |>
    
    # Create column with labels for plot
    mutate(label = case_when(
      is.na(status) == TRUE ~ adjusted,
      .default = status
    )) |>
    
    # Plot DAG
    ggplot(aes(x=x, y=y, xend=xend, yend=yend, color=label)) +
    geom_dag_point() +
    geom_dag_edges_diagonal(edge_alpha = 0.5) +
    geom_dag_label(size = 2.5, show.legend = FALSE) +
    scale_color_manual(breaks = c("adjusted", "exposure", "outcome", "unadjusted"), 
                       values = c("#D55E00", "#009E73", "#56B4E9", NA)) +
    theme_dag()
  
}

sample_innings_runs <- function(data) {
  
  set.seed(1988)
  
  first_innings_sample <- data |>
    
    mutate(
      team_a = batting_team,
      team_b = bowling_team
    ) |>
    
    pivot_longer(batting_team:bowling_team,
                 names_to = "batting",
                 values_to = "team") |>
    mutate(
      opposition = case_when(
        team == team_a ~ team_b,
        team == team_b ~ team_a
      ),
      
      batting = case_when(
        batting == "batting_team" ~ 1,
        .default = 0
      )
    ) |>
    
    group_by(match_id, team) |>
    arrange(innings) |>
    mutate(
      total_runs = case_when(
        batting == 0 ~ lead(total_runs),
        .default = total_runs
      )
    ) |>
    select(match_id, innings, team, batting, opposition, total_wickets_lost, total_overs_faced, total_runs, winning_team, toss_winner, DLS) |>
    filter(innings == 1) |>
    ungroup() |>
    slice_sample(prop = 0.1)
  
}

model_innings_runs <- function(sample) {
  model_first_innings_sample <- glmer(total_runs ~ factor(batting) + (1|match_id) + (1|team) + (1|opposition),
                        family = "poisson",
                        data = sample)
}