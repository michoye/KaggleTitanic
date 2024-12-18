# JS - 2024-18-12

# Function to extract and group titles
create_title_groups <- function(data) {
  data |> 
    mutate(
      title = str_extract(Name, ' ([A-Za-z]+)\\.'),
      title_group = case_when(
        title %in% c(" Capt.", " Col.", " Major.") ~ "Officer",
        title %in% c(" Jonkheer.", " Don.", " Sir.", " Dr.", " Rev.", " Countess.", " Lady.") ~ "Royalty",
        title %in% c(" Mme.", " Ms.", " Mrs.") ~ "Mrs",
        title %in% c(" Mlle.", " Miss.") ~ "Miss",
        title == " Mr." ~ "Mr",
        title == " Master." ~ "Master",
        TRUE ~ title
      )
    )
}