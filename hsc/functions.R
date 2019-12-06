make_tbl_heading = function(string){
  string %>%
    # trim_txt[[6]] %>% 
    str_replace_all("\\s+", " ") %>% 
    str_replace_all("([A-Z])", "\t \\0") %>% 
    str_replace_all("S\t D", "SD") %>% 
    str_replace_all("\t Course", "Course")
}
########################################################
pad_to_p = function(string, num_tabs, side){
  # tmp = trim_txt[[7]] %>%
  tmp = string %>%
    str_replace_all("\\s+", " ") %>% 
    str_replace_all("\\s\\d", "\t\\0")
  
  
  if(str_count(tmp, "\t") < num_tabs){
    if(side == "left"){
      tmp = paste0(paste0(rep(" \t ", num_tabs - str_count(tmp, "\t")),collapse = ""), tmp)
    }
    
    if(side == "right"){
      tmp = paste0(tmp, paste0(rep(" \t ", num_tabs - str_count(tmp, "\t")),collapse = ""))
    }
  }
  tmp = paste0(tmp, " \n")
  return(tmp)
}
########################################################

make_df_from_page = function(this_page){
  ## Trimming white spaces
  trim_txt <- str_split(this_page, "\n", simplify = TRUE) %>% 
    purrr::map(trimws)
  
  ## Find location of title
  which_title = trim_txt %>% 
    purrr::map_lgl(
      .f = ~ str_detect(.x, pattern = "Type of mark")) %>% 
    which()
  
  ## Get precise title
  title = make_tbl_heading(trim_txt[[which_title]])
  
  ## Number of tabs in the table
  (num_tabs = title %>%
      str_count("\t"))
  
  
  ## Initialise an empty list to store the result 
  tmp = vector(mode = "list", length(trim_txt))
  
  ## Go through every trimmed text
  for(i in 1:length(trim_txt)){
    
    ## If we haven't gone to the table, we will just fill the list element up with empty tabs, if we reaches the colnames of the table, we will pad it up with the correct number of tabs
    if(i <= which_title){
      tmp[[i]] = pad_to_p(trim_txt[[i]],  num_tabs = num_tabs, side = "right")
      next
    }
    
    ## If we are in the interior of the table, we have two cases
    if(str_count(trim_txt[[i]], "\\d") >= 10){
      ## If the row has a lot of numbers, this correspond to either the HSC or the scale marks rows. In this case, we will pad tabs from the left. 
      tmp[[i]] = pad_to_p(trim_txt[[i]],  num_tabs = num_tabs, side = "left")
    } else {
      
      ## If the row does not have a lot of numbers, this correspond to the row specifying the course names and number of enrolment. In this case, we will pad from the right with tabs. 
      tmp[[i]] = pad_to_p(trim_txt[[i]],  num_tabs = num_tabs, side = "right")
    }
  }
  
  result = tmp[(which_title+1L):length(tmp)]
  
  result_df = purrr::map_dfr(
    .x = result, 
    .f = ~ read.delim(text = .x, header = FALSE, sep = "\t") %>% 
      dplyr::mutate_all(as.character)) %>% 
    janitor::remove_empty(which = "rows")
  
  colnames(result_df) = strsplit(title, split = " \t ")[[1]]
  
  result_df = result_df %>% as_tibble()
  return(result_df)
}

#######################################################
make_pretty_result_df = function(result_df){
  result_df %>% 
    dplyr::filter(!str_detect(Course, "Report on the Scaling")|is.na(Course)) %>% 
    dplyr::mutate(
      lead1 = dplyr::lead(Course, 1),
      lag1 = dplyr::lag(Course, 1),
      course_cleaned = coalesce(Course, lead1, lag1),
      Number = paste0(Number, `Type of mark`) %>% 
        str_extract(pattern = "[[:digit:]]+") %>% 
        as.integer(),
      lead1 = dplyr::lead(Number, 1),
      lag1 = dplyr::lag(Number, 1),
      number_cleaned = coalesce(Number, lead1, lag1),
      numNA = rowSums(is.na(.)),
      keep = numNA < 8
    ) %>% 
    dplyr::filter(keep) %>% 
    dplyr::select(course_cleaned,
                  number_cleaned,
                  `Type of mark`:`P25`)
}