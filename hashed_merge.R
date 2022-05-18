library(tidyverse)
library(matrixStats)
library(magrittr)
library(timeR)

########## Condition sets ##########
# Docstring:
#' Class representing a single condition (i.e. perfect match, fuzzy match) in a hashed merge.
#' 
#' @field column_name The name of the column to which the condition will be applied. The actual columns that will
#' be looked for are column_name + '_left' and column_name + '_right'.
#' @field perfect A logical representing whether this is a perfect (TRUE) or fuzzy (FALSE) match
#' @field min_score For fuzzy matches, the minimum score which will be considered a match. The convenience constructor
#' further down defaults this to 1. Ignored for perfect matches.
Condition <- setRefClass(
  'Condition',
  fields=list(field_name='character', perfect='logical', min_score='numeric'),
  methods=list(
    evaluate=function(.self, merge_fields, df){
      "Given a datatable, actually evaluate the match the Condition specifies. If a fuzzy match,
      assumes that the match has already been scored and that the score is an appropriately-named column in df.
      Returns a logical vector of length nrow(df)."
      # Get the MergeField object for this field
      this_merge_field <- merge_fields[[.self$field_name]]
      # First consider perfect matches
      if(.self$perfect){
        # If it's not compound, just return whether the column with the same name as the field_name matches
        if(!(this_merge_field$is_compound)){
          return(df[[paste0(.self$field_name, '_left')]] == df[[paste0(.self$field_name, '_right')]])
        }else{
          # It's compound, so need to check that all the partial fields are equal
          # Calculate whether the left and right values are equal for each partial, and implicitly columnwise
          # bind them with simplify=TRUE
          partials_equal <- sapply(
            this_merge_field$partial_fields,
            function(column_name) df[[paste0(column_name, '_left')]] == df[[paste0(column_name, '_right')]],
            simplify=TRUE,
            USE.NAMES=FALSE
            )
          # Essentially return the rowwise `all` of the dataframe of partial equality vectors
          # Treat NAs as False, which is what we want
          return(rowSums(partials_equal, na.rm=TRUE) == ncol(partials_equal))
        }
      }else{
        # Assumes that the match has already been scored
        # Return whether the score is at least the min_score specified
        return(df[[to_score_colname(.self$field_name)]] >= .self$min_score)
      }
    }
  ))


# Docstring:
#' Class representing a set of Conditions (or Condition_sets) in a hashed merge. This is intended to be an abstract
#' class in that
#' it shouldn't be implemented directly; N_of_conditions and All_of_conditions inherit from this and are the ones
#' that should actually be used.
#' 
#' As alluded to above, Condition_sets can contain either Conditions or other Condition_sets in their conditions
#' field. The score and evaluate methods it implements handle this by recursively applying the method to each
#' object in conditions until all of the objects are Conditions (not Condition_sets).
#' 
#' @field conditions A list of Conditions or Condition_sets, to which the subclass-specific logic will be applied to
#' determine whether the condition set is satisfied.
Condition_set <- setRefClass(
  'Condition_set',
  fields=list(conditions='list')
)


# Docstring:
#' Class representing a set of conditions with the logic that n of them must be true for the set of conditions to be
#' considered satisfied. Inherits from Condition_set.
#' @field conditions See Condition_set
#' @field n The number of conditions that must be true for the condition set to be considered satisfied
N_of_conditions <- setRefClass(
  'N_of_conditions',
  contains='Condition_set',  # Inherits from Condition_set
  fields=list(n='numeric'),
  methods=list(
    evaluate=function(merge_fields, df){
      "To evaluate the condition set, call the evaluate method of each object in its conditions field. We anticipated
      this 'duck-typing' by giving both Condition_set and Condition a method named evaluate, so you can just call
      every object's evaluate method and the object itself will worry about which one it is.
      
      n_of_helper sums the matrix returned by sapply into a single vector of length nrow(df) of the number of
      TRUE conditions in the condition set, and then returns whether that is >= n."
      n_of_helper(n, sapply(conditions, function(x)x$evaluate(merge_fields, df), USE.NAMES=FALSE))
    }
  )
)


# Docstring:
#' Class representing a set of conditions with the logic that all of them must be true for the set of conditions to be
#' considered satisfied. Inherits from Condition_set.
#' @field conditions See Condition_set
All_of_conditions <- setRefClass(
  'All_of_conditions',
  contains='Condition_set',  # Inherits from Condition_set
  methods=list(
    evaluate=function(merge_fields, df){
      "To evaluate the condition set, call the evaluate method of each object in its conditions field. We anticipated
      this 'duck-typing' by giving both Condition_set and Condition a method named evaluate, so you can just call
      every object's evaluate method and the object itself will worry about which one it is.
      
      n_of_helper sums the matrix returned by sapply into a single vector of length nrow(df) of the number of
      TRUE conditions in the condition set, and then returns whether that is == the number of conditions."
      # Get a matrix of booleans where each column represents whether one condition is met
      condition_bools <- sapply(conditions, function(x)x$evaluate(merge_fields, df), USE.NAMES=FALSE)
      # Count the total number of columns in the matrix and pass that as n to n_of_helper
      n_of_helper(ncol(condition_bools), condition_bools)
    }
  )
)


########## Convenience functions to class constructors ##########

# These functions are all just convenient ways to create objects of the classes we defined above. These should
# be favored over the actual constructors themselves (so call perfect_match to define a perfect match, rather than
# Condition$new(perfect=TRUE...).) They make sure all of the arguments to the constructors are named, and fuzzy match
# is created with a default min_score of 1.

perfect_match <- function(field_name){
  Condition$new(field_name=field_name, perfect=TRUE)
}

fuzzy_match <- function(field_name, min_score=1){
  Condition$new(field_name=field_name, perfect=FALSE, min_score=min_score)
}

n_of_conditions <- function(n, ...){
  N_of_conditions$new(n=n, conditions=list(...))
}

all_of_conditions <- function(...){
  All_of_conditions$new(conditions=list(...))
}


########## Class definitions ##########

MergeField <- setRefClass(
  'MergeField',
  fields=list(name='character', partial_fields='list', is_compound='logical', scoring_function='function'),
  methods=list(
    column_names=function(.self){
      colnames <- unlist(.self$partial_fields)
      if(!(.self$is_compound)){
        # Add the name of the field if this isn't a compound field
        colnames <- c(.self$name, colnames)
      }
      # Add the bad flag
      c(colnames, to_bad_flag_colname(.self$name))
    },
    df_contains_field=function(.self, df){
      # The dataframe contains all of the needed columns if it has all of the field's partial field, and 
      # the field is either a compound field (in which case the partial fields encompass all of the needed
      # columns) or it has a column with the same name as the field
      # (Eg, for dob we just need columns called (dob_y, dob_m, dob_d), and don't need one called `dob`;
      # but for fn we need (fn1l, fn4l, fn_sdx), *and* fn itself)
      all(sapply(.self$partial_fields, function(x) x %in% colnames(df))) &
        (.self$is_compound | (.self$name %in% colnames(df))) &
        # Also need to check the bad flag is in here
        (to_bad_flag_colname(.self$name) %in% colnames(df))
    },
    score=function(.self, merged_df){.self$scoring_function(merged_df)}
  )
)


HashedMerge <- setRefClass(
  'HashedMerge',
  fields=list(left_df='ANY', right_df='ANY', merge_fields='list'),
  methods=list(
    score=function(.self, merged_df){
      scores <- sapply(.self$merge_fields, function(field) field$score(merged_df), simplify=FALSE, USE.NAMES=TRUE)
      # Rename them to have _score suffixes so that we can just bind them directly to the dataframe in the round
      # method below
      names(scores) <- to_score_colname(names(scores))
      scores
    },
    round=function(.self, join_keys, other_conditions=All_of_conditions(conditions=list()), drop_bad=c()){
      #' Function to conduct a hashed merged between two datasets.
      #' @param left_df One of the two datasets to be merged. If either of the two datasets is considered to be the "spine"
      #' dataset, it should be passed here. Should be a data.table.
      #' @param right_df The other dataset to be merged. Should be a data.table
      #' @param join_keys list/vector of field names on which the datasets will be joined (i.e. two observations must be
      #' a perfect match on all of these fields to be considered a match candidate.) "dob" is acceptable here and will
      #' be split out into its component parts by the function.
      #' @param other_conditions A Condition_set representing other conditions, in addition to matching on the join_keys,
      #' that must be satisfied for a pair of observations to be considered a match candidate
      #' @param drop_bad list/vector of field names that should be dropped if the corresponding "bad flag" is TRUE. These
      #' should be the field names themselves, not the bad flag column names. "dob" is acceptable here.
      
      # Start timer
      timer <- createTimer(verbose=FALSE)
      timer$start('hashed_merge')
      print(sprintf('Starting hashed merge round at %s', Sys.time()))
      
      # Create a named merge_fields list with the field names as the list entry names
      # (This might have been the case anyway but we check for it here basically bc initialize methods in
      # ReferenceClasses, where it would make more sense to do this, are kind of a pain)
      # Since we're using this object, we'll make reference to merge_fields rathe than .self$merge_fields
      # throughout the code
      merge_fields <- list()
      for(field in .self$merge_fields){
        merge_fields[[field$name]] <- field
      }

      # The dataframes are gonna get copied on assignment in a sec anyway, so just do it now so that we don't
      # have to keep making calls to .self
      left_df <- .self$left_df
      right_df <- .self$right_df

      # Print info about size of datasets
      print(sprintf('Left dataset (as passed to hashed_merge_round) has %d records.', nrow(left_df)))
      print(sprintf('Right dataset (as passed to hashed_merge_round) has %d records.', nrow(right_df)))
      
      # Get a vector of the names of the flag columns we're filtering by
      bad_flags <- sapply(drop_bad, to_bad_flag_colname, USE.NAMES=FALSE)
      # Select those columns and drop where any of the bad flags are true
      left_df <- left_df %>% filter(!if_any(all_of(bad_flags)))
      right_df <- right_df %>% filter(!if_any(all_of(bad_flags)))
      # Print information about size of datasets after filtering
      print(sprintf('Left dataset has %d records after dropping bad records.', nrow(left_df)))
      print(sprintf('Right dataset has %d records after dropping bad records.', nrow(right_df)))

      # Deduplicate on PII and ID
      # Get list of all of the PII fields and bad flags we'll be using
      all_pii_fields_and_bad_flags <- sapply(
        merge_fields,
        function(field) field$column_names(),
        simplify=TRUE,
        USE.NAMES=FALSE
      ) %>% unlist() %>% unname()
      left_df <- left_df %>% distinct(id, across(all_of(all_pii_fields_and_bad_flags)))
      right_df <- right_df %>% distinct(id, across(all_of(all_pii_fields_and_bad_flags)))
      # Print more info
      print(sprintf('Left dataset has %d records after deduplication.', nrow(left_df)))
      print(sprintf('Right dataset has %d records after deduplication.', nrow(right_df)))
      
      # Join the two datasets on the join keys (i.e. on required perfect matches)
      # First need to handle join keys for compound fields, i.e. 'dob' as a join key does not have a single
      # field in the dataset called 'dob'
      join_key_columns <- .self$merge_fields[join_keys] %>%
        sapply(function(field) unlist(if(field$is_compound) field$partial_fields else field$name)) %>%
        unlist() %>%
        unname()
      # The na_matches is redundant if bad_flags contains the flags for all of the join keys, but include it just
      # to be safe
      merged <- inner_join(
        left_df,
        right_df,
        by=join_key_columns,
        suffix=c('_left', '_right'),
        keep=TRUE,
        na_matches='never'
      )
      # The other_conditions part will complain if there weren't any matches
      # so check for that case explicitly
      if(nrow(merged) == 0){
        return(merged)
      }
      
      # Irrespective of if any other conditions were passed, score on all the PII fields
      scores <- .self$score(merged)
      merged <- bind_cols(merged, scores)
      # Check if any other conditions were passed
      has_other_conditions <- length(other_conditions$conditions) > 0
      if(has_other_conditions){
        # Evaluate the other conditions required for a match
        meets_other_conditions <- other_conditions$evaluate(merge_fields, merged)
        # Filter to candidate matches that meet the required other conditions
        matches <- merged %>% filter(meets_other_conditions)
      }else{
        # Anything that merged on the join keys is a match
        matches <- merged
      }
      # Calculate max score and number of righthand match candidates within the join keys and left ID
      matches <- matches %>%
        # Shouldn't need the na.rm=TRUE here, but doesn't hurt
        mutate(total_score=rowSums(across(ends_with('_score')), na.rm=TRUE)) %>%
        # Move the ID columns, scores to the front of the df
        select(id_left, id_right, total_score)
      
      # Print some summary stats
      timer$stop('hashed_merge')
      elapsed_time <- timer$getEvent('hashed_merge')$timeElapsed
      cat(sprintf('Hashed merge round completed at %s.\n%d matches found (%d after deduplication).\n%s elapsed.\n',
                  Sys.time(),
                  nrow(matches),
                  matches %>% summarize(n_distinct(id_left, id_right)) %>% pull(),
                  lubridate::seconds_to_period(elapsed_time)))
      # Return the dataframe of results
      return(matches)
    }
  )
)


DEFAULT_LN_SCORE_FUNCTION <- function(df){
   case_when(
        # Full match is 102 points
        df$ln_left == df$ln_right ~ 102,
        # Soundex and first 4 match is 72 points
        (df$ln_sdx_left == df$ln_sdx_right) &
           (df$ln4l_left == df$ln4l_right) ~ 72,
        # First 4 match is 52 points
        df$ln4l_left == df$ln4l_right ~ 52,
        # Soundex and first letter match is 32 points
        (df$ln_sdx_left == df$ln_sdx_right) &
           (df$ln1l_left == df$ln1l_right) ~ 32,
        # Soundex match is 22 points
        df$ln_sdx_left == df$ln_sdx_right ~ 22,
        # First letter match is 12 points
        df$ln1l_left == df$ln1l_right ~ 12,
        # None of the above is 0 points
        TRUE ~ 0  
      )
}

DEFAULT_FN_SCORE_FUNCTION <- function(df){
   case_when(
        # Full match is 101 points
        df$fn_left == df$fn_right ~ 101,
        # Soundex and first 4 match is 71 points
        (df$fn_sdx_left == df$fn_sdx_right) &
           (df$fn4l_left == df$fn4l_right) ~ 71,
        # First 4 match is 51 points
        df$fn4l_left == df$fn4l_right ~ 51,
        # Soundex and first letter match is 31 points
        (df$fn_sdx_left == df$fn_sdx_right) &
           (df$fn1l_left == df$fn1l_right) ~ 31,
        # Soundex match is 21 points
        df$fn_sdx_left == df$fn_sdx_right ~ 21,
        # First letter match is 11 points
        df$fn1l_left == df$fn1l_right ~ 11,
        # None of the above is 0 points
        TRUE ~ 0  
      )
}

DEFAULT_SSN_SCORE_FUNCTION <- function(df){
  partial_scores <- sapply(seq(1, 8), function(digit1){
    # Digit1 is the first digit in the partial column name (e.g. "ssn12", "ssn23" etc), so use that to figure
    # the name of this partial SSN column
    partial_ssn_name <- paste0('ssn', digit1, digit1+1)
    # Save the left and right column names as as variables so we can use the .. notation
    left_partial_name <- paste0(partial_ssn_name, '_left')
    right_partial_name <- paste0(partial_ssn_name, '_right')
    # Return a vector of whether the _left and _right values are equal for this partial SSN column
    df %>% pull(.data[[left_partial_name]]) == df %>% pull(.data[[right_partial_name]])
  }, USE.NAMES=FALSE) 
  # 9 points if it's a perfect match, otherwise one point for each equal partial
  # Multiply the old version of the score by 12 to make it comparable to FN/LN/DOB
  if_else(df %>% pull(ssn_left) == df %>% pull(ssn_right), 9, rowSums(partial_scores, na.rm=TRUE)) * 12
}

DEFAULT_DOB_SCORE_FUNCTION <- function(df){
  # DOB scores
  case_when(
    (  # All fields match is 100 points
      (df$dob_d_left == df$dob_d_right) &
        (df$dob_m_left == df$dob_m_right) &
        (df$dob_y_left == df$dob_y_right)
    ) ~ 100,
    (  # Month-year match is 33 points
      (df$dob_m_left == df$dob_m_right) &
        (df$dob_y_left == df$dob_y_right)
    ) ~ 33,
    (  # Day-year match is 23 points
      (df$dob_d_left == df$dob_d_right) &
        (df$dob_y_left == df$dob_y_right)
    ) ~ 23,
    (  # Month-day match is 13 points
      (df$dob_m_left == df$dob_m_right) &
        (df$dob_d_left == df$dob_d_right)
    ) ~ 13,
    TRUE ~ 0  # None of the above is 0 points
  )
}


DEFAULT_FN_MERGE_FIELD <- MergeField(
  name='fn',
  partial_fields=list('fn4l', 'fn1l', 'fn_sdx'),
  is_compound=FALSE,
  scoring_function=DEFAULT_FN_SCORE_FUNCTION
)
DEFAULT_LN_MERGE_FIELD <- MergeField(
  name='ln',
  partial_fields=list('ln4l', 'ln1l', 'ln_sdx'),
  is_compound=FALSE,
  scoring_function=DEFAULT_LN_SCORE_FUNCTION
)
DEFAULT_SSN_MERGE_FIELD <- MergeField(
  name='ssn',
  partial_fields=list('ssn12', 'ssn23', 'ssn34', 'ssn45', 'ssn56', 'ssn67', 'ssn78', 'ssn89'),
  is_compound=FALSE,
  scoring_function=DEFAULT_SSN_SCORE_FUNCTION
)
DEFAULT_DOB_MERGE_FIELD <- MergeField(
  name='dob',
  partial_fields=list('dob_y', 'dob_m', 'dob_d'),
  is_compound=TRUE,
  scoring_function=DEFAULT_DOB_SCORE_FUNCTION
)


default_hashed_merge <- function(left_df, right_df, fn=TRUE, ln=TRUE, ssn=TRUE, dob=TRUE){
  merge_fields <- list()
  if(fn){merge_fields[['fn']] <- DEFAULT_FN_MERGE_FIELD}
  if(ln){merge_fields[['ln']] <- DEFAULT_LN_MERGE_FIELD}
  if(ssn){merge_fields[['ssn']] <- DEFAULT_SSN_MERGE_FIELD}
  if(dob){merge_fields[['dob']] <- DEFAULT_DOB_MERGE_FIELD}
  HashedMerge(
    left_df=left_df,
    right_df=right_df,
    merge_fields=merge_fields
  )
}


########## OO helper functions ##########

n_of_helper <- function(n, eval_matrix){
  #' Just a helper for rowwise summing of a matrix of booleans and whether the result is >= n.
  
  # Essentially treat NAs as False, which is what we want
  return(rowSums(eval_matrix, na.rm=TRUE) >= n)  # Assumes this comes out of an sapply
}

is_condition <- function(maybe_condition){
  #' Returns whether an object is a Condition. Used to differentiate Conditions and Condition_sets in some of the
  #' methods above.
  return('column_name' %in% names(maybe_condition))
}

to_bad_flag_colname <- function(field){
  #' Given a field name, return the standardized column name representing the flag for whether that value is bad.
  return(paste0('bad_', field))
}

to_score_colname <- function(field){
  #' Given a field name, return the standardized column name representing that field's score column name.
  return(paste0(field, '_score'))
}

get_condition_column_names <- function(condition_or_condition_set){
  #' Given a Condition or Condition_set, return all of the column name encompassed by that object. This is used to get
  #' the total set of columns contained in a Condition_set, it would be nonsensical to pass it a Condition directly
  #' (but the function itself does this as it recurses over whatever object you pass it, hence why it accepts them.)
  if(is_condition(condition_or_condition_set)){
    # Ie if it's a column, just return its name
    condition_or_condition_set$column_name
  }else{
    # Ie if it's a condition set, recursively apply this function to each of its conditions/condition sets
    unique(sapply(condition_or_condition_set$conditions, function(maybe_condition){
      get_condition_column_names(maybe_condition)
    }))
  }
}


########## Merge utility functions ##########

iteratively_collapse_column <- function(df, column_to_collapse, fixed_columns){
  # Docstring:
  #' Function for creating an ID that identifies groups of observations that are linked by transitively by equality
  #' across either of two sets of features. Equivalently, finds separable sets of observations where edges are
  #' defined as equality between two nodes of either column_to_collapse or ALL of fixed_columns.
  #' 
  #' E.g., when creating surviving IDS: all occurrences of a set of PII need to have the same surviving ID,
  #' and all occurrences of an original ID also need to have the same surviving ID. Iterates back and forth, setting
  #' surviving ID by each set of columns, until the aforementioned condition is satisfied.
  #' Used for constructing "surviving" and "CPL" IDs. The actual ID chosen is the minimum value of column_to_collapse
  #' within each transitively connected group.
  #' 
  #' Returns the dataframe that was passed, with a new column named 'collapsed_{column_to_collapse}' added.
  #' 
  #' @field df The dataframe to which the collapsed column should be added
  #' @field column_to_collapse Column to be collapsed; this and fixed_columns are interchangeable, EXCEPT that
  #'   column_to_collapse expects one column name and is the column from which the collapsed ID is drawn
  #' @field fixed_columns Set of columns defining the other feature by which the edges are constructed. This
  #'   expects a vector of strings, even if it is only one column.
  
  # Start timer
  timer <- createTimer(verbose=FALSE)
  timer$start('surviving_ids')
  
  collapsed_column <- paste0('collapsed_', column_to_collapse)
  
  # Initialize variables  
  n_rounds_set_by_fixed <- 0
  n_rounds_set_by_column_to_collapse <- 0
  df_collapsed <- df %>% mutate('{collapsed_column}':=.data[[column_to_collapse]])
  
  print('Beginning collapsed ID algorithm...')
  # Check if fixed columns ever have multiple values in the collapsing column, i.e. if we even need to do anything
  fixed_has_multiple_collapsed_ids <- df_collapsed %>%
    group_by(across(all_of(fixed_columns))) %>%
    filter(n_distinct(.data[[collapsed_column]]) > 1) %>%
    n_groups() > 0
  
  # We essentially alternate between setting surviving ID by PII and by original ID, until they're both 1:1 with
  # surviving ID
  while(fixed_has_multiple_collapsed_ids){
    # Give each set of values in the fixed columns a single value in the column to collapse
    print('Setting collapsed ID by fixed columns')
    df_collapsed <- df_collapsed %>%
      group_by(across(all_of(fixed_columns))) %>%
      arrange(.data[[collapsed_column]]) %>%
      mutate('{collapsed_column}':=first(.data[[collapsed_column]])) %>%
      ungroup()
    n_rounds_set_by_fixed <- n_rounds_set_by_fixed + 1
    
    column_to_collapse_has_many_collapsed <- df_collapsed %>%
      group_by(across(all_of(column_to_collapse))) %>%
      filter(n_distinct(.data[[collapsed_column]]) > 1) %>%
      nrow() > 0
    
    if(!column_to_collapse_has_many_collapsed){
      # We're done!
      break()
    }
    
    # Otherwise, fix the column to collapse's many-ness to the collapsed column
    print('Setting collapsed ID by (original) column to collapse')
    df_collapsed <- df_collapsed %>%
      group_by(across(all_of(column_to_collapse))) %>%
      arrange(.data[[collapsed_column]]) %>%
      mutate('{collapsed_column}':=first(.data[[collapsed_column]])) %>%
      ungroup()
    n_rounds_set_by_column_to_collapse <- n_rounds_set_by_column_to_collapse + 1
    
    # Calculate if the fixed columns now have multiple values of the collapsed column,
    # and then let the while loop check if we actually need to do this again
    fixed_has_multiple_collapsed_ids <- df_collapsed %>%
      group_by(across(all_of(fixed_columns))) %>%
      filter(n_distinct(.data[[collapsed_column]]) > 1) %>%
      nrow() > 0
  }
  
  # Print some info
  timer$stop('surviving_ids')
  elapsed_time <- timer$getEvent('surviving_ids')$timeElapsed
  
  print(sprintf('Collapsed IDs algorithm completed at %s.', Sys.time()))
  print(sprintf('Collapsed ID was set by fixed columns %d times.', n_rounds_set_by_fixed))
  print(sprintf('Collapsed ID was set by original column to be collapsed %d times.',
                n_rounds_set_by_column_to_collapse))
  print(sprintf('In total the algorithm took %f "rounds".', 0.5*(n_rounds_set_by_fixed +
                                                                   n_rounds_set_by_column_to_collapse)))
  print(sprintf('Algorithm took %s.', lubridate::seconds_to_period(elapsed_time)))
  
  # Return the dataframe
  df_collapsed
}


########## General utility functions ##########


write_file_hashed_merge <- function(df, output_name, ftype, dir='./',
                                    prefix='', suffix='', dated_copy=TRUE){
  # Function for saving results, both a dated copy and overwriting the "current" version
  
  # Create partial function for writing CSVs that writes NAs as empty strings
  my_write_csv <- function(...){write_csv(..., na='')}
  # Get function to write out the file
  write_function <- list('csv'=my_write_csv, 'rds'=write_rds)[[ftype]]
  # Check for the case that it's not a type we have a function for
  if(is.null(write_function)){
    stop(sprintf('Unknown file type "%s"', ftype))
  }
  # Write "current" version
  write_function(df, file.path(dir, paste0(prefix, output_name, suffix, '.', ftype)))
  if(dated_copy){
    # Write "dated" version
    # Will break if a "dated" folder doesn't exist
    write_function(df, file.path(dir, 'dated',  paste0(prefix, output_name, suffix, '_', Sys.Date(), '.', ftype)))
  }
}

write_csv_hashed_merge <- function(...){
  write_file_hashed_merge(..., ftype='csv')
}

write_rds_hashed_merge <- function(...){
  write_file_hashed_merge(..., ftype='rds')
}


########## The star(s) of the show ##########

run_default_cpl_hashed_merge <- function(left, right, left_name, right_name, write=TRUE,
                                         dir='./', prefix='', suffix=''){
  # Write a single function to do the entire hashed merge process for 2 agencies using their names

  # Define the hashed merge object
  hashed_merge <- default_hashed_merge(left_df=left, right_df=right)

  # Round 0: Perfect match on all fields
  round_0 <- hashed_merge$round(join_keys=c('ssn', 'fn', 'ln', 'dob'), drop_bad=c('ssn', 'fn', 'ln', 'dob'))
  
  # Round 1: Perfect match on ssn, perfect match on 2 of fn/ln/dob, and fuzzy
  # match on whichever didn't perfect match
  round_1 <- hashed_merge$round(join_keys=c('ssn'), other_conditions=all_of_conditions(
    # This is a little roundabout but I think it's the easiest way to represent "and fuzzy match on whatever didn't
    # perfect match"
    n_of_conditions(
      2,
      perfect_match('fn'),
      perfect_match('ln'),
      perfect_match('dob')
    ),
    all_of_conditions(
      fuzzy_match('fn', 1),
      fuzzy_match('ln', 1),
      fuzzy_match('dob', 1)
    )
  ),
  drop_bad=c('ssn', 'fn', 'ln', 'dob')
  )
  
  # Round 2: Perfect match on ssn, perfect match on 2 of fn/ln/dob
  round_2 <- hashed_merge$round(
    join_keys=c('ssn'),
    other_conditions=n_of_conditions(
      2,
      perfect_match('ln'),
      perfect_match('fn'),
      perfect_match('dob')
    ),
    drop_bad=c('ssn')
  )
  
  # Round 4: Perfect match on ssn, perfect match on one of fn, ln, or DOB
  round_4 <- hashed_merge$round(
    join_keys=c('ssn'),
    other_conditions=n_of_conditions(
      1,
      perfect_match('ln'),
      perfect_match('fn'),
      perfect_match('dob')
    ),
    drop_bad=c('ssn')
  )
  
  all_rounds <- list(
    `0`=round_0,
    `1`=round_1,
    `2`=round_2,
    `4`=round_4
  )
  
  generate_match_outputs(all_rounds, left_name, right_name, write=write,
                         dir=dir, prefix=prefix, suffix=suffix)
}


########## Post-match outputs ##########

generate_match_outputs <- function(all_rounds, left_name, right_name, write_crosswalk=TRUE,
                                   dir='./', prefix='', suffix=''){
  
  # We use a sort of dirty trick throughout this - the names of all_rounds should be string representations of the
  # the round number, and since we may have dropped out some of our original rounds and it's confusing to renumber
  # them, they may not be sequential, may not start at 0, etc (though they should be monotonically increasing).
  # We'll index into the list using *string* representations of the round number
  # when we want to pull by round number, and *integers* when we just want the nth element of the list. So it's
  # very important to follow this convention when indexing into the list!!
  
  save_closure <- function(df, output_name){
    # Just define this once to make things easier
    write_csv_hashed_merge(df, paste(left_name, right_name, output_name, sep='_'), dir=dir,
                           prefix=prefix, suffix=suffix)
  }
  
  # Add a column for round number to the dataframe for each round
  all_rounds <- mapply(function(df, round_num){df %>% mutate(round=as.numeric(round_num))},
                       all_rounds, names(all_rounds), SIMPLIFY=FALSE)
  num_rounds <- length(all_rounds)  # Just useful to have
  
  # Initialize empty tibble for the crosswalk (just get the colnames from the first element of all_rounds)
  seen_matches <- head(all_rounds[[1]], 0)
  
  # Initialize empty lists for match retentions and round summaries
  match_retentions <- list()
  round_summaries <- list()
  
  # This produces a waterfall table - outer loop iterates from 0 to 11, inner loop iterates from outer loop to 11
  for(original_round_ix in seq(1, num_rounds)){
    # Get the matches from the original round (using a string version of the round number to index into all_rounds)
    original_round <- all_rounds[[original_round_ix]] %>% distinct(id_left, id_right, .keep_all=TRUE)
    # Pull the *new* unique matches from the original round's total matches
    original_new_matches <- original_round %>%
      anti_join(seen_matches, by=c('id_left', 'id_right'))
    # Of the new unique matches, how many of the members of those pairs were matched to other records in prior rounds?
    round_summaries[[names(all_rounds)[[original_round_ix]]]] <- list(
      total_matches=nrow(original_round),
      new_matches=nrow(original_new_matches),
      unique_left_ids=original_new_matches %>% distinct(id_left) %>% nrow(),
      unique_right_ids=original_new_matches %>% distinct(id_right) %>% nrow(),
      seen_left_ids=original_new_matches %>% select(id_left) %>% distinct() %>%
        inner_join(seen_matches %>% select(id_left) %>% distinct()) %>% nrow(),
      seen_right_ids=original_new_matches %>% select(id_right) %>% distinct() %>%
        inner_join(seen_matches %>% select(id_right) %>% distinct()) %>% nrow(),
      this_round_left_id_multiple_matches=original_round %>%
        group_by(id_left) %>%
        summarize(n_right_ids=n_distinct(id_right)) %>%
        summarize(sum(n_right_ids > 1)),
      this_round_right_id_multiple_matches=original_round %>%
        group_by(id_right) %>%
        summarize(n_left_ids=n_distinct(id_left)) %>%
        summarize(sum(n_left_ids > 1))
    )
    
    # Save the round summary field names bc they get all messed up by the data gynmastics later
    round_summary_colnames <- names(round_summaries[[1]])
    
    # Retention waterfall
    match_retention <- sapply(seq(original_round_ix, num_rounds), function(this_round_ix){
      # Get the distinct matches from this round
      this_round <- all_rounds[[this_round_ix]] %>% select(id_left, id_right) %>% distinct()
      # Want the matches from this round that were also in the original round, so do an inner join and count rows
      return(this_round %>% inner_join(original_new_matches, by=c('id_left', 'id_right')) %>% nrow())
    })
    # Pad the bottom of the matrix with NAs
    match_retentions[[original_round_ix]] <- c(rep(NA, times=original_round_ix-1), match_retention)
    # Add this round's new matches to seen_matches
    seen_matches <- seen_matches %>% bind_rows(original_new_matches)
  }
  
  # Now actually create the crosswalk
  crosswalk_df <- all_rounds %>%
    # Bind all the matches together
    bind_rows() %>%
    # Get the max score and min round for each ID pair (i.e. the best match for that pair)
    group_by(id_left, id_right) %>%
    summarize(score=max(total_score), round=min(round)) %>%
    ungroup()

  # Clean up and save the round summaries
  round_summaries <- data.frame(matrix(unlist(round_summaries), nrow=length(round_summaries), byrow=TRUE))
  # Use the colnames we saved from earlier
  colnames(round_summaries) <- round_summary_colnames
  round_summaries <- round_summaries %>% mutate(round_number=names(all_rounds)) %>% relocate(round_number)
  round_summaries %>% save_closure('round_summaries')
  
  
  # Clean up and save the retention waterfall
  retention_waterfall <- as_tibble(t(simplify2array(match_retentions)))
  round_names <- paste0('Round ', names(all_rounds))
  colnames(retention_waterfall) <- round_names
  retention_waterfall <- retention_waterfall %>% mutate('Original round'=round_names) %>% relocate('Original round')
  retention_waterfall %>% save_closure('waterfall')

  crosswalk_df <- crosswalk_df %>%
    # Only want to actually write out distinct pairs of IDs and the round in which they were matched
    select("{left_name}_id":=id_left, "{right_name}_id":=id_right, round, score)
  
  if(write_crosswalk){
    crosswalk_df %>% save_closure('crosswalk')
  }
  
  crosswalk_df
}
