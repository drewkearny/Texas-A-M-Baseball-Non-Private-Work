# Load in NCAA PBP Version

Year <- get_ncaa_schedule_info(team_id = 697, year = 2022)
Yearpbp <- list()
for (i in 1:nrow(Year)) {
  Yearpbp[[i]] <- get_ncaa_baseball_pbp(Year$game_info_url[i])
}

Tamu22 = plyr::ldply(Yearpbp,data.frame)

Tamu22 <- Tamu22 %>% 
  rename(Date = game_date, Inning = inning, Top.Bottom = inning_top_bot,) %>% 
  select(Date, Inning, Top.Bottom, score, description, year)


Tamu22$Inning <- as.numeric(Tamu22$Inning)

# Adding Batter column
Tamu22$batter <- word(string = Tamu22$description, start = 1, end = 2)
#remove unnecessary strings in the batter column
Tamu22$batter <- str_remove(Tamu22$batter,"struck|singled|reached|fouled|flied|grounded|
                           homered|hit|walked.|walked|advanced|doubled|popped|foul|out|tripled|lined|
                           intentionally|singled,|infield|walked3a|homered,")
#remove unncessary leading and trailing strings in the batter column
Tamu22$batter <- str_squish(Tamu22$batter)

#if the string detected is a slash or to p for, the corresponding batter row should be NA
Tamu22$batter <- ifelse(str_detect(Tamu22$description, "to p for")==TRUE,NA,Tamu22$batter)
Tamu22$batter <- ifelse(str_detect(Tamu22$description, "\\/")==TRUE,NA,Tamu22$batter)

Tamu22 <- subset(Tamu22, 
                 !(grepl("^R:|^E:|^H:|^LOB:| pinch$| to$", batter, ignore.case = TRUE))&
                   !is.na(batter)
)

Tamu22 <- Tamu22 %>%
  group_by(Date, Inning, Top.Bottom) %>%
  mutate(batter_order = row_number())



# Function to calculate r1_name
calculate_r1_name <- function(data) {
  m <- nrow(data)
  r1_name <- character(m)
  clear_bases <- TRUE  # Initialize with bases empty
  prev_description <- "start of the game"  # Initialize as "start of the game" for the first row
  prev_inning <- data$Inning[1]  # Store the initial inning
  prev_top_bottom <- data$Top.Bottom[1]  # Store the initial top/bottom value
  
  for (i in 1:(m-1)) {  # Updated the loop range to go up to m-1
    current_play <- data[i, ]
    
    if (clear_bases || prev_description == "start of the game" || prev_inning != current_play$Inning || prev_top_bottom != current_play$Top.Bottom) {
      r1_name[i] <- ""
    } else {
      # Split the current description by semicolon to handle multiple players
      player_descriptions <- unlist(strsplit(current_play$description, ";"))
      # Take the part of the description before the first semicolon
      current_play_description <- trimws(player_descriptions[1])
      
      # Remove content within parentheses (counts) from the description
      current_play_description <- gsub("\\(.*?\\)", "", current_play_description)
      
      # Extract player names from the modified description
      names <- unique(gsub(".*?([A-Z]+).*?(?:;|$)", "\\1", current_play_description))
      names <- names[names != "RBI"]  # Remove "RBI"
      names <- trimws(names)  # Remove leading and trailing spaces
      
      if (length(names) > 0) {
        # Check keywords in the description to determine player's position
        if (grepl("singled|walked|hit by pitch|reached", current_play_description) && !grepl("doubled|tripled|homered|advanced|scored|out|stole", current_play_description)) {
          r1_name[i+1] <- names[1]  # Assign the first player's name to r1_name
        } else if (grepl("reached first", current_play_description) && grepl("struck out", current_play_description)) {
          r1_name[i+1] <- names[1]  # Assign the first player's name to r1_name
        } else if ((r1_name[i] == '' || !grepl("advanced to second|stole second|advanced to third|stole third|scored|out", r1_name[i])) && !grepl("double play|advanced to second|stole second|advanced to third|stole third|scored|caught stealing|picked off|homered", current_play_description)) {
          r1_name[i+1] <- r1_name[i]  # Keep the current r1_name
        } else if (grepl("singled|doubled|tripled|advanced to second|stole second|advanced to third|stole third|scored|homered|out at second", current_play_description)) {
          r1_name[i+1] <- ""  # Clear r1_name
        }
      }
    }
    
    if (i < m && (current_play$Inning != data$Inning[i + 1] || current_play$Top.Bottom != data$Top.Bottom[i + 1])) {
      clear_bases <- TRUE  # Bases are cleared at the end of an inning or when Top.Bottom changes
    } else {
      clear_bases <- FALSE
    }
    
    prev_description <- current_play$description
    prev_inning <- current_play$Inning
    prev_top_bottom <- current_play$Top.Bottom
  }
  
  return(data.frame(r1_name))
}







# Function to calculate r2_name
calculate_r2_name <- function(data) {
  m <- nrow(data)
  r2_name <- character(m)
  bases <- character(3)  # Initialize bases with empty strings
  prev_description <- "start of the game"  # Initialize as "start of the game" for the first row
  prev_inning <- data$Inning[1]  # Store the initial inning
  prev_top_bottom <- data$Top.Bottom[1]  # Store the initial top/bottom value
  
  for (i in 1:(m-1)) {  # Updated the loop range to go up to m-1
    current_play <- data[i, ]
    
    if (prev_description == "start of the game" || prev_inning != current_play$Inning || prev_top_bottom != current_play$Top.Bottom) {
      # Initialize bases with empty strings at the beginning of each inning or top/bottom
      bases <- character(3)
    }
    
    # Split the current description by semicolon to handle multiple players
    player_descriptions <- unlist(strsplit(current_play$description, ";"))
    
    for (desc in player_descriptions) {
      # Remove content within parentheses (counts) from the description
      desc <- gsub("\\(.*?\\)", "", desc)
      
      # Extract player names from the modified description
      names <- unique(gsub(".*?([A-Z]+).*?(?:;|$)", "\\1", desc))
      names <- names[names != "RBI"]  # Remove "RBI"
      names <- trimws(names)  # Remove leading and trailing spaces
      
      if (length(names) > 0) {
        # Check keywords in the description to determine player's position
        if (grepl("doubled|advanced to second|stole second", desc) && !grepl("advanced to third|scored|out|stole third", desc)) {
          # Assign the first player's name to r2_name and place on second base
          r2_name[i+1] <- names[1]
          bases[2] <- names[1]
        } else if ((r2_name[i] == '' || !grepl("stole third|advanced to third|scored|out", r2_name[i])) && grepl("double play", desc)) {
          # Clear r2_name if double play occurred
          r2_name[i+1] <- ""
        }
      }
    }
    
    # Clear bases for players who scored or homered
    for (j in 1:3) {
      if (grepl(paste0("\\b", bases[j], "\\b scored\\.|\\b", bases[j], "\\b homered\\."), prev_description)) {
        bases[j] <- ""
      }
    }
    
    if (i < m && (current_play$Inning != data$Inning[i + 1] || current_play$Top.Bottom != data$Top.Bottom[i + 1])) {
      # Bases are cleared at the end of an inning or when Top.Bottom changes
      bases <- character(3)
    }
    
    prev_description <- current_play$description
    prev_inning <- current_play$Inning
    prev_top_bottom <- current_play$Top.Bottom
  }
  
  return(data.frame(r2_name))
}

# Function to calculate r3_name
calculate_r3_name <- function(data) {
  m <- nrow(data)
  r3_name <- character(m)
  bases <- character(3)  # Initialize bases with empty strings
  prev_description <- "start of the game"  # Initialize as "start of the game" for the first row
  prev_inning <- data$Inning[1]  # Store the initial inning
  prev_top_bottom <- data$Top.Bottom[1]  # Store the initial top/bottom value
  
  for (i in 1:(m-1)) {  # Updated the loop range to go up to m-1
    current_play <- data[i, ]
    
    if (prev_description == "start of the game" || prev_inning != current_play$Inning || prev_top_bottom != current_play$Top.Bottom) {
      # Initialize bases with empty strings at the beginning of each inning or top/bottom
      bases <- character(3)
    }
    
    # Split the current description by semicolon to handle multiple players
    player_descriptions <- unlist(strsplit(current_play$description, ";"))
    
    for (desc in player_descriptions) {
      # Remove content within parentheses (counts) from the description
      desc <- gsub("\\(.*?\\)", "", desc)
      
      # Extract player names from the modified description
      names <- unique(gsub(".*?([A-Z]+).*?(?:;|$)", "\\1", desc))
      names <- names[names != "RBI"]  # Remove "RBI"
      names <- trimws(names)  # Remove leading and trailing spaces
      
      if (length(names) > 0) {
        # Check keywords in the description to determine player's position
        if (grepl("tripled|advanced to third|stole third", desc) && !grepl("scored|out", desc)) {
          # Assign the first player's name to r3_name and place on third base
          r3_name[i+1] <- names[1]
          bases[3] <- names[1]
        } else if ((r3_name[i] == '' || !grepl("scored", r3_name[i])) && grepl("scored|stole home|homered", desc)) {
          # Clear r3_name if player scored or homered
          r3_name[i+1] <- ""
        }
      }
    }
    
    # Clear bases for players who scored or homered
    for (j in 1:3) {
      if (grepl(paste0("\\b", bases[j], "\\b scored\\.|\\b", bases[j], "\\b homered\\."), prev_description)) {
        bases[j] <- ""
      }
    }
    
    if (i < m && (current_play$Inning != data$Inning[i + 1] || current_play$Top.Bottom != data$Top.Bottom[i + 1])) {
      # Bases are cleared at the end of an inning or when Top.Bottom changes
      bases <- character(3)
    }
    
    prev_description <- current_play$description
    prev_inning <- current_play$Inning
    prev_top_bottom <- current_play$Top.Bottom
  }
  
  return(data.frame(r3_name))
}


r1_name <- calculate_r1_name(Tamu22)
r2_name <- calculate_r2_name(Tamu22)
r3_name <- calculate_r3_name(Tamu22)

# Combine the results with your original dataframe
tamu22combo <- cbind(Tamu22, r1_name, r2_name, r3_name)

