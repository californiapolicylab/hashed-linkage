library(tidyverse)
library(matrixStats)
library(magrittr)
library(timeR)

########## Condition sets ##########
# Docstring:
#' Class representing a single condition (i.e. perfect match, fuzzy match) in a hashed merge. Looks for columns suffixed
#' with `_left` and `_right` for each column invovlved in evaluating this condition. This naming convention is applied
#' automatically in conducting a round of a HasheMerge.
#' 
#' @param field_name The name of the MergeField to which the condition will be applied (i.e. the piece of PII
#'   this condition pertains to.)
#' @param perfect A logical representing whether this is a perfect (TRUE) or fuzzy (FALSE) match
#' @param min_score For fuzzy matches, the minimum score which will be considered a match. The convenience constructor
#'   further down defaults this to 1. Ignored for perfect matches.
Condition <- setRefClass(
  'Condition',
  fields=list(field_name='character', perfect='logical', min_score='numeric'),
  methods=list(
    evaluate=function(.self, merge_fields, df){
      "Given a dataframe and set of MergeFields, actually evaluate the match the Condition specifies. If a fuzzy match,
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
          # If it's compound, need to check that all the partial fields are equal
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
#' object in `conditions` until all of the objects are Conditions (not Condition_sets).
#' 
#' @param conditions A list of Conditions or Condition_sets, to which the subclass-specific logic will be applied to
#'   determine whether the condition set is satisfied.
Condition_set <- setRefClass(
  'Condition_set',
  fields=list(conditions='list')
)


# Docstring:
#' Class representing a set of conditions with the logic that n of the conditions must be true for the set of conditions to be
#' considered satisfied. Inherits from Condition_set.
#' @param conditions See Condition_set
#' @param n The number of conditions that must be true for the condition set to be considered satisfied
N_of_conditions <- setRefClass(
  'N_of_conditions',
  contains='Condition_set',  # Inherits from Condition_set
  fields=list(n='numeric'),
  methods=list(
    evaluate=function(merge_fields, df){
      "To evaluate the condition set, call the evaluate method of each object in its conditions field. We anticipated
      this 'duck-typing' by giving both Condition_set and Condition a method named evaluate, so you can just call
      every object's evaluate method and the object itself will worry about which one it is.
      
      n_of_helper sums the matrix returned by sapply into a single vector of length nrow(df) containing the number of
      TRUE conditions in the condition set, and then returns whether that is >= n."
      n_of_helper(n, sapply(conditions, function(x)x$evaluate(merge_fields, df), USE.NAMES=FALSE))
    }
  )
)


# Docstring:
#' Class representing a set of conditions with the logic that all of them must be true for the set of conditions to be
#' considered satisfied. Inherits from Condition_set.
#' @param conditions See Condition_set
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

# Docstring:
#' Class representing a piece of PII that will be linked on. Importantly, this class represents a *piece of PII*, not
#' *a single column*. For instance, you would create a single MergeField object for each of first name, last name,
#' date of birth, SSN, address, etc. These objects contain the logic needed to score and evaluate conditions on this
#' piece of PII. The documentation for the arguments should help clarify the information contained in this object:
#' 
#' @param name The name of the field as a whole, e.g. `fn`, `ln`, `address`. If this field is not compound (see the 
#'   `is_compound` argument below for an explanation), this value must match the name of the column containing the full value
#'   of this piece of PII.
#' 
#' @param partial_fields The "partial" PII fields derived from this field. If this field is compound, this will be
#'   the names of all of the columns that comprise the field; if it is not, it will contain the names of the columns
#'   derived from the full value. This should be a list, not a vector, of strings.
#' 
#' @param is_compound A boolean indicating whether this is a _compound_ field. We define a compound field as one in which
#'   there is no single field that can be checked for exact equality of the field as a whole. For example, first name
#'   is _not_ compound, because we will have a single `fn` field that contains the full value of the field. However,
#'   date of birth or address _are_ compound, because date of birth must be broken into year, month, and day, and because
#'   address will be broken into any number of components. For compound fields, exact equality is checked by checking
#'   _all_ of the field's `partial_fields`.
#' 
#' @param scoring_function A function used to score fuzzy matches on this field. It should take one argument, a dataframe,
#'   and return a vector of scores with the same length as the dataframe. Each row of the dataframe can be assumed
#'   to contain two values for this PII field, which we are scoring in relation to each other. The columns
#'   corresponding to each value can be accessed with the name of the column in the base data, suffixed by
#'   `_left` and `_right`, e.g. `fn_left` and `fn_right`.
MergeField <- setRefClass(
  'MergeField',
  fields=list(name='character', partial_fields='list', is_compound='logical', scoring_function='function'),
  methods=list(
    column_names=function(.self){
      #' Return a list of the column names encompassed by this field. Useful for checking the presence of all necesary
      #' columns in the data, or generally for selecting all of the columns pertaining to this piece of PII.'
      colnames <- unlist(.self$partial_fields)
      if(!(.self$is_compound)){
        # Add the name of the field itself if this isn't a compound field
        colnames <- c(.self$name, colnames)
      }
      colnames
    }
  )
)


# Docstring:
#' Class representing the configuration of the hashed merge to be conducted. Encapsulates the datasets themselves, and all information
#' needed to score matches on each PII field. The `round` method is used to conduct a single round of a hashed merge.
#' 
#' @param left_df The left dataset. It does not matter which dataset is the left vs the right, but you should keep track of which is which.
#' 
#' @param right_df The "right" dataset.
#' 
#' @param merge_fields A list (not vector) of `MergeField`s. This list should be comprehensive of all of the PII fields on which
#'   the two datasets will be merged.
HashedMerge <- setRefClass(
  'HashedMerge',
  fields=list(left_df='ANY', right_df='ANY', merge_fields='list'),
  methods=list(
    score=function(.self, merged_df){
      #' Score all of the PII fields used in this hashed merge. Takes in a dataframe containing two copies of each column needed
      #' across all `MergeField`s, one suffixed with `_left` and one suffixed with `_right`. This is done as part of the `round`
      #' method; the dataframe passed to this method is the result of joining the two datasets on the `join_keys` for a round.
      
      # Call the scoring function for each merge field
      scores <- sapply(.self$merge_fields, function(field) field$scoring_function(merged_df), simplify=FALSE, USE.NAMES=FALSE)
      # Rename them to have _score suffixes so that we can just bind them directly to the dataframe in the round
      # method below
      names(scores) <- sapply(.self$merge_fields, function(mf) mf$name %>% to_score_colname()) %>% unlist() %>% unname()
      scores
    },
    round=function(.self, join_keys, other_conditions=All_of_conditions(conditions=list()), drop_bad=c()){
      #' Method to conduct a single round of a hashed merge between two datasets.
      #' @param join_keys list/vector of `MergeField` names on which the datasets will be joined (i.e. two observations must be
      #'   a perfect match on all of these `MergeField`s to be considered a match candidate.)
      #' @param other_conditions A Condition_set representing other conditions, in addition to matching on the join_keys,
      #'   that must be satisfied for a pair of observations to be considered a match candidate. These are evaluated after
      #'   joining the two datasets by the field indicated in `join_keys`. Must be a `Condition_set`, not a `Condition`. If
      #'   you would like to pass a single condtion, wrap it in an `All_of_conditions`.
      #' @param drop_bad list/vector of columns names indicating whether a record should be dropped. Put another way:
      #'   if any of the columns listed in `drop_bad` is `TRUE` for a given row, that record will be excluded from this
      #'   merge round.

      # Start timer
      timer <- createTimer(verbose=FALSE)
      timer$start('hashed_merge')
      print(sprintf('Starting hashed merge round at %s', Sys.time()))
      
      # Create a named merge_fields list with the field names as the list entry names
      # (This might have been the case anyway but we check for it here basically bc initialize methods in
      # ReferenceClasses, where it would make more sense to do this, are kind of a pain)
      # Since we're using this object, we'll make reference to merge_fields rathe than .self$merge_fields
      # throughout the code
      named_merge_fields <- list()
      for(field in .self$merge_fields){
        named_merge_fields[[field$name]] <- field
      }
      
      # The dataframes are gonna get copied on assignment in a sec anyway, so just do it now so that we don't
      # have to keep making calls to .self
      left_df <- .self$left_df
      right_df <- .self$right_df
      
      # Before we try to do anything, want it to break if all of the necessary columns are not present
      # Get list of all of the PII fields and bad flags we'll be using
      all_pii_fields_and_bad_flags <- c(sapply(
        named_merge_fields,
        function(field) field$column_names(),
        simplify=TRUE,
        USE.NAMES=FALSE
      ) %>% unlist() %>% unname(), drop_bad)
      required_columns <- c('id', all_pii_fields_and_bad_flags)
      left_missing_columns <- required_columns[!(required_columns %in% colnames(left_df))]
      right_missing_columns <- required_columns[!(required_columns %in% colnames(right_df))]
      # Create a list of error messages, which will only be populated if there are any issues
      err_messages <- c()
      if(length(left_missing_columns) > 0){
        err_messages <- c(err_messages, sprintf('Left dataset mising columns %s', left_missing_columns))
      }
      if(length(right_missing_columns) > 0){
        err_messages <- c(err_messages, sprintf('Right dataset mising columns %s', right_missing_columns))
      }
      if(length(err_messages) > 0){
        # If either dataset is missing any required columns, raise an informative error
        stop(paste(c(err_messages, 'Check your datasets and the MergeFields you are passing to this HashedMerge'), sep=';'))
      }
      
      # Make sure the bad flags are logicals, since filter will complain otherwise
      left_df <- left_df %>% mutate(across(all_of(drop_bad), as.logical))
      left_df <- right_df %>% mutate(across(all_of(drop_bad), as.logical))
      
      # Print info about size of datasets
      print(sprintf('Left dataset (as passed to hashed_merge_round) has %d records.', nrow(left_df)))
      print(sprintf('Right dataset (as passed to hashed_merge_round) has %d records.', nrow(right_df)))
      
      # Drop where any of the bad flags are true
      # For some reason if you pass an empty vector to all_of here it will drop all of the data, so only do
      # this logic if there are any drop_bad flags
      if(length(drop_bad) > 0){
        left_df <- left_df %>% filter(!if_any(all_of(drop_bad)))
        right_df <- right_df %>% filter(!if_any(all_of(drop_bad)))
        # Print information about size of datasets after filtering
        print(sprintf('Left dataset has %d records after dropping bad records.', nrow(left_df)))
        print(sprintf('Right dataset has %d records after dropping bad records.', nrow(right_df)))
      }

      # Deduplicate on PII and ID
      left_df <- left_df %>% distinct(id, across(all_of(all_pii_fields_and_bad_flags)))
      right_df <- right_df %>% distinct(id, across(all_of(all_pii_fields_and_bad_flags)))
      # Print more info
      print(sprintf('Left dataset has %d records after deduplication.', nrow(left_df)))
      print(sprintf('Right dataset has %d records after deduplication.', nrow(right_df)))
      
      # Join the two datasets on the join keys (i.e. on required perfect matches)
      # First need to handle join keys for compound fields, i.e. 'dob' as a join key does not have a single
      # field in the dataset called 'dob'
      join_key_columns <- named_merge_fields[join_keys] %>%
        sapply(function(field) unlist(if(field$is_compound) field$partial_fields else field$name)) %>%
        unlist() %>%
        unname()
      # The na_matches is to ensure we never match missings to missings (unlike SQL, the inner_join function
      # will do that by default if you don't specify na_matches)
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
        meets_other_conditions <- other_conditions$evaluate(named_merge_fields, merged)
        # Filter to candidate matches that meet the required other conditions
        matches <- merged %>% filter(meets_other_conditions)
      }else{
        # Anything that merged on the join keys is a match
        matches <- merged
      }
      # Calculate total score across all PII fields
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


########## Default linkage strategy ##########

# This section contains the default scoring functions and MergeFields for CPL's default linkage strategy.
# See the readme for more detail on the strategy itself.


# Scoring functions for the four default MergeFields
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


# The four default MergeFields themselves
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


default_hashed_merge_object <- function(left_df, right_df, fn=TRUE, ln=TRUE, ssn=TRUE, dob=TRUE){
  #' Function to return a HashedMerge object corresponding to the default linkage strategy, given two datasets.
  #' Can also subset the default fields using the four flags in the signature. Note that this just returns a
  #' HashedMerge object; it does not actually run a hashed merge, or specify anything about the default rounds.
  #' See the `run_default_cpl_hashed_merge` function to do this.
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


########## Helper functions ##########

n_of_helper <- function(n, eval_matrix){
  #' Just a helper for rowwise summing of a matrix of booleans and whether the result is >= n.
  
  # Essentially treat NAs as False, which is what we want
  return(rowSums(eval_matrix, na.rm=TRUE) >= n)  # Assumes this comes out of an sapply
}

to_score_colname <- function(field){
  #' Given a field name, return the standardized column name representing that field's score column name.
  return(paste0(field, '_score'))
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
  #' @param df The dataframe to which the collapsed column should be added
  #' @param column_to_collapse Column to be collapsed; this and fixed_columns are interchangeable, EXCEPT that
  #'   column_to_collapse expects one column name and is the column from which the collapsed ID is drawn
  #' @param fixed_columns Set of columns defining the other feature by which the edges are constructed. This
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
  #' Function for saving results, with the option to save a dated copy as well.
  #' `ftype` can be 'csv' or 'rds'.
  
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

run_default_cpl_hashed_merge <- function(left, right, left_name, right_name,
                                         write=TRUE, dir='./', prefix='', suffix=''){
  #' A single function to run the default CPL linkage strategy on two datasets. Does not allow any customization from the default;
  #' if this is desired, it can be done straightforwardly using the MergeField and HashedMerge objects.
  #' @param left The left dataset. It does not matter which dataset is the right vs the left, but you should keep
  #'   track of which is which.
  #' @param right The right dataset.
  #' @param left_name The name of the left dataset. This is only used in naming the ID columns in the final crosswalk output.
  #' @param right_name The name of the right dataset.
  #' @param write Logical indicating whether the results should actually be written to disk.
  #' @param dir Directory to which the results should be written.
  #' @param prefix Prefix which will be prepended to output filenames.
  #' @param suffix Suffix which will be appended to output filenames.

  # Define the hashed merge object
  hashed_merge <- default_hashed_merge_object(left_df=left, right_df=right)

  # Round 0: Perfect match on all fields
  round_0 <- hashed_merge$round(join_keys=c('ssn', 'fn', 'ln', 'dob'), drop_bad=c('bad_ssn', 'bad_fn', 'bad_ln', 'bad_dob'))
  
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
  drop_bad=c('bad_ssn', 'bad_fn', 'bad_ln', 'bad_dob')
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
    drop_bad=c('bad_ssn')
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
    drop_bad=c('bad_ssn')
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
  #' Function to creat a single crosswalk, round summaries, and waterfalls for the rounds of a hashed linkage.
  #' See the readme for more documentation on the outputs themselves.
  #' 
  #' @param all_rounds A list of the outputs of individual linkage rounds. If the list is named, those names will
  #'   be used as the round names, otherwise they will be numbered sequentially.
  #' @param left_name Name of the left dataset.
  #' @param right_name Name of the right dataset.
  #' @param write_croswalk A boolean indicating whether the crosswalk should be written to disk. Included because
  #'   it is sometimes useful to not
  #'   overwrite an existing file when debugging, but usage of the `suffix` flag to avoid this is preferable to using this flag.
  #'   This flag may
  #'   be deprecated at some point in future.
  #' @param dir Directory to which the results should be written.
  #' @param prefix Prefix which will be prepended to output filenames.
  #' @param suffix Suffix which will be appended to output filenames.
  
  save_closure <- function(df, output_name){
    # Just define this once to make things easier
    write_csv_hashed_merge(df, paste(left_name, right_name, output_name, sep='_'), dir=dir,
                           prefix=prefix, suffix=suffix)
  }
  
  # If there are no names on the rounds list, just number them sequentially
  if(is.null(names(all_rounds))){
    names(all_rounds) <- seq(1, length(all_rounds))
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
