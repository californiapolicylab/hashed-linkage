# CPL Hashed Linkage Toolkit

This is the repository of code described in the California Policy Lab's [Hashed Linkages for
Administrative Datasets](https://www.capolicylab.org/connecting-families-to-benefits-using-linked-data-a-toolkit/) guide. There are broadly two steps to a hashed linkage
(described in detail in the guide), hashing and
linkage. This respository contains code for both of these steps. The files intended for use by an end user are
as follows:

##### Hashing
* `hashing_base_linkagetoolkit.sas` (SAS)
* `python_hashing_config_template.ini` (Language-agnostic)

##### Linkage
* `hashed_merge_template.Rmd` (R)
* `hashed_merge_template_minimal.Rmd` (R)

Either the SAS or python code can be used for the hashing process, according to the users's
preferred language. While the python code itself should not need to be adjusted (all parameters are specified in the .ini file), and
can therefore be run by someone mostly unfamiliar with python, it does require a suitable python environment, which will likely require
some knowledge of python to setup. As indicated by the documentation in the SAS script and .ini file, only the names of the PII fields
and the most recent acceptable year for DOB should actually need to be changed by the end user.

This code is designed to be extensible to arbitrary pieces of PII, though this requires some programming in R. For users less familiar with
R, or those whose PII is some subset of first name, last name, SSN, and date of birth (the "default" fields for which this code was originally
developed), there are sensible defaults that can be used that require minimal programming by the end user, described in the `Linkage` section below.


# Prerequisites

There are a number of aspects of a hashed linkage that must be worked out beforehand for the process to proceed smoothly. Broadly, one
should know the PII available in the data to be linked, as well as how it is going to be linked. Developing the scoring and matching rounds
before conducting the hashing of the data allows the user to know what "partial" PII fields need to be created, what cleaning steps
make the most sense, and how to flag bad data.

### PII fields
Having a list of each PII field in the data, as well as any components/partial fields derived from it, will make the development of the scoring schemes and rounds
much simpler (though there may be some back-and-forth as, for instance, desired levels of precision when constructing the matching rounds may necessitate the
creation of new fields derived from the unhashed PII.) This list could look as follows:

* First name: full first name, first two letters of first name, first four letters of first name, phonetic transcription
  of first name
* ...
* Date of birth: year of birth, month of birth, day of birth

You should also decide on criteria for what constitutes "bad" data in a given field, though the code can be used without this.

### Scoring schemes
You will need to develop a scoring scheme (or use an already-constructed default) for each PII field contained in the datasets. By "scoring scheme", we mean
a point value assigned to each of the varying strengths of fuzzy matches. For instance, if two first names do not exactly match,
we would still think that they are more similar if they have the same first four letters and phonetic transcription
than if they only have the same first letter. Developing the scoring scheme for a field is just translating this
introspection or prior knowledge about a field in the data into point values. **This package contains default scoring
schemes for first name, last name, SSN, and date of birth**; you should feel free to use these if your hashed data
contains the necessary fields and if you do not have any a priori opinion on the ordering of strengths of fuzzy matches. 
The rest of this section assumes that you will be developing your own scoring scheme.

The order of the possible scores is more important than the points values themselves, 
but it is generally advisable to go from a score for a perfect
match of around 100 to a score for the least similarity that could still be considered a fuzzy match of about 10. For
instance, the default scoring scheme for first name is as follows:

* Match on full first name: 101 points
* Match on soundex and first four letters: 71 points
* Match on first four letters: 51 points
* Match on soundex and first letter: 31 points
* Match on soundex: 21 points
* Match on first letter: 11 points

Any two first names that don't have any of the above matches are given a score of zero points. Translating this
conceptual scheme into code is covered below in the "Usage" section.

### Matching rounds
You will also need to develop a set of matching rounds that will constitute your hashed linkage. A round is a set of
criteria (perfect match on a field, fuzzy match on two fields, etc,) that will constitute one level of stringency for
possible matches. The ultimate output will contain all matches found in any round, with each pair of IDs also including
the highest score and earliest round in which the match was found. These rounds should be in
at least roughly descending order of stringency. At this stage, a conceptual description is fine. In addition to
matching criteria, each round should include a list of fields where rows with bad data in any of those fields should be
excluded. Like the scoring schemes, **this package contains a default set of rounds for a hashed merge on FN, LN, DOB,
and SSN**. As an example of what rounds look like, the rounds used in the default merge are as follows:

* Round 0: Perfect match on SSN, FN, LN, DOB; exclude bad SSNs, FNs, LNs, DOBs
* Round 1: Perfect match on SSN and two of (FN, LN, DOB), and any fuzzy match on the third; exclude bad SSNs, FNs, 
  LNs, DOBs
* Round 2: Perfect match on SSN and two of (FN, LN, DOB); exclude bad SSNs
* Round 3: Perfect match on SSN and one of (FN, LN, DOB); exclude bad SSNs

The eagle-eyed reader will note that any match captured in round 0 is necessarily also captured in all later rounds.
In fact, round 3 will necessarily find all of the matches found in any of the earlier rounds. This is admittedly
partly an artifact of earlier iterations of the code. However, this may not always be the case; later rounds may
have criteria that are not strictly looser than earlier rounds. Further, even if it is the case, you too may want
to have rounds that are technically redundant, as it adds only minimally to the computing time and allows you to
quickly filter matches by quality in the output, as the ultimate crosswalk produced will indicate the round in which
each match was _first_ found.


# Hashing

The hashing code is built around some "default" specifications for the above conceptual prerequisites. It is available in
either python or SAS, which produce the same output on any given dataset (with a minimal exception noted further down in this
section). If your partial features, cleaning procedures, or definition of "bad" data differ from what is described here,
you will need to edit the hashing code for whichever language you are using. The rest of this section will proceed
assuming this is not the case, and that the hashing code is appropriate for your linkage as-is.


## Cleaning procedures

The cleaning procedures for each of the four PII fields is as follows:

* First name
    - Convert characters with accents or diacritics to 26-character alphabet characters
    - Convert to lowercase
    - Strip whitespace at ends
    - Remove prefixes "Mr", "Mrs", "Dr", and "Ms"
    - Remove all non-a-to-z characters
* Last name
    - Same as first name, except rather than removing prefixes the suffixes "Jr" and "Sr" are removed
* SSN
    - Remove all non-numeric characters
    - Pad the left side with zeroes until it is 9 digits long
* Date of birth
    - None


## Derived/"partial" features

The following features are created from the full values of each PII field **before** hashing takes place:

* First name (`fn`)
    - First four letters of first name (`fn4l`)
    - First two letters of first name (`fn2l`)
    - Soundex phonetic transcription of first name (`fn_sdx`)
* Last name (`ln`)
    - Same as first name
* SSN (`ssn`)
    - 8 features, each of which is the full SSN with two adjacent digits removed; e.g., `ssn34` is the result
    of removing the third and fourth digits from the full SSN
* DOB
    - Year of birth (`dob_y`)
    - Month of birth (`dob_m`)
    - Day of birth (`dob_d`)
    - (Note that DOB has no "full" value in the output, only these three partial features)


## Bad data flags

Each of the four PII fields also has a flag indicating whether the data for that field in a given record
should be treated as "bad", i.e. of insufficient quality to be used in evaluating matches in the linkage.
The criteria used to determine this flag for each field are as follows:

* First name (`bad_fn`)
    - String is empty after cleaning
* Last name (`bad_ln`)
    - Same as FN
* SSN (`bad_ssn`)
    - String is empty, OR
    - String is longer than 9 characters, OR
    - The first three digits are "000" or "666", OR
    - The fourth and fifth digits are "00", OR
    - The sixth through ninth digits are "0000", OR
    - The first digit is "9", OR
    - The full value is either "078051120" or "123456789", i.e. is a common fake value
    - (All of the above are checked after cleaning)
* Date of birth (`bad_dob`)
    - Any component of date of birth is missing, OR
    - Year of birth is before 1900, OR
    - Year of birth is after the value of `latest_year` set in the python config file or SAS macro call


## Language differences

The SAS and python hashing scripts should produce identical results when run on the same dataset, **with one exception**:
the SAS code uses a slight variant of the true American Soundex algorithm (which is incidentally also used by SQL),
which produces a different phonetic transcription in a very limited number of cases (~.02% in a test dataset of 100M names.)
This is a known issue, but is currently unaddressed.

Other data-specific possible sources of differences for which one may need to account stem from the `proc import` macro in SAS. This scans
the entire dataset to guess the types of columns, but it may still guess incorrectly, particularly for social security numbers. You 
should check the logfile to be sure that the SSN column was read as a string, and quote the SSN values if it was not. Date formats may also introduce
discrepancies; this should be addressed proactively before running the code based on knowledge of how the dates are formatted in the
dataset. The PII columns have their types hardcoded appropriately in the python script, so this should not be an issue if using that
language. The python config file also contains an option to specify the format of dates in the data.


# Linkage

The code used to conduct linkages of data that has been hashed using CPL's hashing code is not yet packaged as
a proper R package, but is primarily a single R file that can be `source`d in a notebook to load the required functions.
The file in question is `hashed_merge.R`. `hashed_merge_template.Rmd` contains an example of all of the below in practice, and may
be useful to look at before reading the rest of this section.


## Data prep and ID creation

You may need to do some cleaning or postprocessing even after hashing the data. The column names must match between the two datasets
(and must match those specified by the default MergeFields further in this document if you are using them). Other typical cleaning and data prep
includes casting columns to the correct types, and/or deduplication of datasets where appropriate. Additionally, the linkage code will expect
a column called `id` in each dataset. This is the ID that will be included in the crosswalk that is the ultimate output of the linkage. **The
linkage does not enforce any limitations as to each ID matching only to a single ID in the other dataset**; put another way, the ultimate
crosswalk should be treated as having a many-to-many relationship between the two datasets' IDs. Depending on how you plan to use the
crosswalk, this may mean that you will want to create IDs in each dataset that uniquely identify each row (or some key that uniquely
identifies the data.) Alternatively, you can do your own postprocessing on the crosswalk to make it 1:1 or 1:M, as suits your usage.


## Usage

Usage of the linkage code consists mainly of translating the conceptual aspects of the linkage outlined in the `Prerequisites` section
of this document into operational definitions in R. We will first define the set of PII fields that we have in our datasets, including how
fuzzy matches on each field should be
scored, and will then proceed to defining the rounds of our hashed linkage. This information and functionality
is encapsulated in two classes created using R's ReferenceClass framework, `MergeField` and `HashedMerge`.


### `MergeField`

A `MergeField` object defines a single PII field, including any "partial" fields derived from it and how fuzzy
matches on this field should be scored. These fields are ultimately passed to a `HashedMerge` object, defined below.
Defining the `MergeField`s will entail translating the conceptual lists of
PII fields and scoring schemes into code. There are four fields that must be defined in creating a `MergeField`:

* `name`: The name of the field as a whole, e.g. `fn`, `ln`, `address`. If this field is not compound (see the 
	`is_compound` field below for an explanation), this value must match the name of the column containing the full value
  of this piece of PII.
* `partial_fields`: The "partial" PII fields derived from this field. If this field is compound, this will be
	the names of all of the columns that comprise the field; if it is not, it will contain the names of the columns
	derived from the full value. This should be a list, not a vector, of strings.
* `is_compound`: A boolean indicating whether this is a _compound_ field. We define a compound field as one in which
	there is no single field that can be checked for exact equality of the field as a whole. For example, first name
	is _not_ compound, because we will have a single `fn` field that contains the full value of the field. However,
	date of birth or address _are_ compound, because date of birth must be broken into year, month, and day, and because
	address will be broken into any number of components. For compound fields, exact equality is checked by checking
	_all_ of the field's `partial_fields`.
* `scoring_function`: A function used to score fuzzy matches on this field. It should take one argument, a dataframe,
	and return a vector of scores with the same length as the dataframe. Each row of the dataframe can be assumed
	to contain two values for this PII field, which we are scoring in relation to each other. The columns
	corresponding to each value can be accessed with the name of the column in the base data, suffixed by
	`_left` and `_right`.

As illustrative examples, we include here the definitions of the default `MergeField`s included for first name,
last name, SSN, and DOB. **If you are using these defaults, the column names in both datasets must match those
in the
 `name` and `partial_fields` fields in the source code below.**

#### First name
```

DEFAULT_FN_MERGE_FIELD <- MergeField(
  name='fn',
  partial_fields=list('fn4l', 'fn1l', 'fn_sdx'),
  is_compound=FALSE,
  scoring_function=function(df){
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
})
```

#### Last name
```

DEFAULT_LN_MERGE_FIELD <- MergeField(
  name='ln',
  partial_fields=list('ln4l', 'ln1l', 'ln_sdx'),
  is_compound=FALSE,
  scoring_function=function(df){
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
})

```

#### SSN
```

DEFAULT_SSN_MERGE_FIELD <- MergeField(
  name='ssn',
  partial_fields=list('ssn12', 'ssn23', 'ssn34', 'ssn45', 'ssn56', 'ssn67', 'ssn78', 'ssn89'),
  is_compound=FALSE,
  scoring_function=function(df){
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
  if_else(df %>% pull(ssn_left) == df %>% pull(ssn_right), 9, rowSums(partial_scores)) * 12
})

```

#### DOB
```

DEFAULT_DOB_MERGE_FIELD <- MergeField(
  name='dob',
  partial_fields=list('dob_y', 'dob_m', 'dob_d'),
  is_compound=TRUE,
  scoring_function=function(df){
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
})

```

### `HashedMerge`

The `HashedMerge` object encapsulates the two datasets that will be merged and the `MergeField`s that they contain.
Its `round` method is used to define and run a single round of a hashed merge, as we will see below.
The fields that are needed to construct a `HashedMerge` are as follows:

* `left_df`: The "left" dataset. It does not matter which of the two datasets is defined as the left dataset
	or the right dataset, though you should keep in mind which is which.
* `right_df`: The "right" dataset.
* `merge_fields`: A list (not vector) of `MergeField`s. This list should be comprehensive of all of the PII fields on which
	the two datasets will be merged.

Once your `MergeField`s are constructed and your datasets are cleaned and read in, it should be trivial to construct
the `HashedMerge` object itself.

### Merge rounds

With the `HashedMerge` object constructed, we can move on to conducting the actual rounds of the linkage. There are
three components to the definition of a round: _join keys_, _other conditions_, and _bad flags_.

#### Join keys

The join keys in a given round are any fields that must _always_ be an exact match for two records to be considered
a match. For example, if your criteria in a given round were "perfect match on FN, LN, and DOB", all three of these
fields would be join keys. A less trivial example would be the criteria "Perfect match on SSN, perfect match on 2/3
of (LN, FN, DOB), and fuzzy match on whichever doesn't perfect match." There is only one field here that must _always_
perfect match, which is SSN, so SSN is the only join key for this round; the other conditions will be addressed in
the `other_conditions` argument.

#### Other conditions

The `other_conditions` in a given round are any criteria for a match that are anything other than always requiring a
perfect match on a specific field (i.e., anything that cannot be represented as a join key.) Under the hood, the package
works by first joining the two datasets on the `join_keys` for speed and to cut down on cardinality, and then filtering
these _candidate matches_ to only those that satisfy the `other_conditions`.

There are two types of objects defined in `hashed_merge.R` that will allow us to translate the conceptual definitions
of rounds into operational definitions that we can pass to `other_conditions`: `Condition`s and `ConditionSet`s.

#### `Condition`s

A `Condition` is a representation of a single criterion for two rows to be considered a match in a hashed merge round.
For instance, a perfect match on first name, represented as `perfect_match('fn')`, or a fuzzy match on last name
with a minimum score of 20, represented as `fuzzy_match('ln', min_score=20)`. These two functions, `perfect_match` and
`fuzzy_match`, are the two functions used to define conditions. If `min_score` is not specified, `fuzzy_match` assumes
a minimum score of 1, i.e. any fuzzy match is considered sufficient.

#### `ConditionSet`s

A `ConditionSet` defines the "and"/"or" logic between `Condition`s within a hashed merge round. There are two functions
used to define `ConditionSet`s: `all_of_conditions`, and `n_of_conditions`. As the names suggest,
the  former only counts two records as a match if all of the conditions passed to it are met, whereas the latter
counts two records as a match if n of the conditions are met. These functions accept an arbitrary number of conditions
(or, as we will see momentarily, other `ConditionSet`s), with `n_of_conditions` also requiring an argument `n` in addition to
all of the other arguments. Some simple examples of translations from conceptual definitions to code are as follows:

* Fuzzy match on first name and last name, with a min score of 20:
`all_of_conditions(fuzzy_match('fn', min_score=20), fuzzy_match('ln', min_score=20))`

* Any fuzzy match on 2/3 of FN, LN, and DOB: `n_of_conditions(fuzzy_match('fn'), fuzzy_match('ln'), fuzzy_match('dob'), n=2)`

* Any fuzzy match only on SSN\*: `all_of_conditions(fuzzy_match('ssn'))`

\* Note that the `other_conditions` argument must always be a `ConditionSet`, not a `Condition`, so even if you
are only passing one condition you must wrap it in an `all_of_conditions`.

As mentioned above, the arguments to `all_of_conditions` and `n_of_conditions` can be not only `Condition`s, but other
`ConditionSet`s; this allows for us to represent more complicated logic. For example, it may not be immediately obvious
how to represent "perfect match on 2/3 of (FN, LN, DOB), and fuzzy match on the third" using this framework. To do so,
we take advantage of the fact that any perfect match is necessarily _also a fuzzy match_, and, by nesting condition
sets, represent these criteria as follows:

```

all_of_conditions(
  # We'll use the nested condition sets mentioned earlier here - (perfect match on 2/3) and (fuzzy match on all 3)
  # are essentially two different sets of conditions, and we need them both to be true, hence wrapping them in an
  # all_of_conditions above
  # First represent the perfect match on 2/3 using n_of_conditions
  n_of_conditions(
    perfect_match('fn'),
    perfect_match('ln'),
    perfect_match('dob'),
    n=2
  ),
  # Now add the "fuzzy match on all 3" condition set
  all_of_conditions(
    fuzzy_match('fn'),
    fuzzy_match('ln'),
    fuzzy_match('dob')
  )
)

```

It is possible to represent quite complicated logic this way. Scores are calculated only once per round irrespective of
the conditions passed, so adding further conditions or condition sets should add to the computation time only minimally.

#### Bad flags (and handling of bad data)

The final component of a matching round is the exclusion of records that have bad data in any fields you specify.
This is specified via the `drop_bad` argument to `round`. `drop_bad` expects a character vector corresponding to the
names of boolean variables that should be `FALSE` if a record should be included, and `TRUE` if it should be
excluded (i.e., if the data is bad.)

The linkage will not treat two missing values as equal, but there are a number of reasons to still use bad flags.
First, they can cut down on the cardinality of the merge significantly, as records that have any of the bad flags
flagged will be excluded from the merge. Second, you may have non-missing values that are known to be placeholders for
mising data. SAS may also fill missing values of hashed PII with the string "2020202...", depending on the
version of the hashing code and the version of SAS that you are using. You can either replace these values with NA
in a preprocessing script before passing it into a `HashedMerge`, or use bad flags to exclude these values. You should be sure
to do one of these two things; **without addressing these values from SAS in one of these ways, two missing values will be evaluated as equal.**
Bad flags are also useful if you have values of hashed PII that you know to be "bad" in some way but you wish to preserve, perhaps
for future examination. Rather than replacing these values with NA and losing them, or creating some extra column
with the original values preserved, you can flag them as bad. Third, you can use bad flags to exclude records that may
have valid data for the fields being linked on in a given round but which you want to exclude due to their having bad data
in another field, or for some other reason.

It obviously will not be known to the linkage code whether you created any bad flag columns as part of the hashing
process or after the fact, but for e.g. the case of a known placeholder value you would need to set the flag before
hashing the PII, as after hashing you cannot know which values are the placeholder.

#### Example

The code defining the rounds in `run_default_cpl_hashed_merge` is as follows:

```

# Define the hashed merge object
hashed_merge <- default_hashed_merge(left_df=left, right_df=right)

# Round 0: Perfect match on all fields
round_0 <- hashed_merge$round(join_keys=c('ssn', 'fn', 'ln', 'dob'), drop_bad=c('ssn', 'fn', 'ln', 'dob'))

# Round 1: Perfect match on ssn, perfect match on 2 of fn/ln/dob, and fuzzy
# match on whichever didn't perfect match
round_1 <- hashed_merge$round(join_keys=c('ssn'), other_conditions=all_of_conditions(
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

# Round 3 omitted for historical reasons

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

```


### Generating the final outputs

To create a single crosswalk from the results of a series of rounds, and output some further diagnostics about each round, pass a list of
the outputs from the rounds to to the `generate_match_outputs` function. This outputs three files:

* `[Left agency name]_[Right agency name]_crosswalk.csv`: The main crosswalk, mapping IDs from the left dataset to IDs in the right dataset
  and vice versa. Besides the IDs themselves, it contains columns indicating the earliest round and highest score for which each pair of IDs
  was found as a match. (The earliest/highest is because an ID may be mapped to multiple sets of PII, and so may match to an ID in the other
  dataset multiple times across or within rounds, and with different scores.)
* `[Left agency name]_[Right agency name]_round_summaries.csv`: A table of diagnostics for the matches found in each round. This includes
  information on the total number of matches, number of new (i.e. not found in previous rounds) matches, and the cardinality of matches.
* `[Left agency name]_[Right agency name]_waterfall.csv`: A "waterfall" indicating the number of matches found in each round, broken out
  by the round in which the match was originally found. If your rounds go in order of strictly loosening criteria (i.e. each round
  should find all of the matches in all rounds before it), then only the diagonal of this table will be meaningful, and will just be the
  number of new matches found in each round. This table is sometimes redundant with the round summaries, but is useful to quickly scan.

It accepts the following arguments:

* `all_rounds`: A `list` of the outputs from individual rounds. If the list is not named the rounds will be assumed to be in sequential order.
  If the list is named, these will be used as the names of the rounds in the output. 
* `left_name`, `right_name`: The names of the left and right datasets. These will be used just to name the columns in the crosswalk
  of the form `[left_name]_id` and `[right_name]_id`, so that it is clear which ID is from which dataset.
* `write_crosswalk`: A boolean indicating whether the crosswalk should be written to disk. Included because it is sometimes useful to not
  overwrite an existing file when debugging, but usage of the `suffix` flag to avoid this is preferable to using this flag. This flag may
  be deprecated at some point in future.
* `dir`, `prefix`, and `suffix`: Strings indicating the path and filenames to which the output should be written. The three outputs
  described above will be written to `[dir]/[prefix][output name][suffix].csv`.


## Full example

For a complete example of usage of the linkage code, see `hashed_merge_template.Rmd`. For a greatly simplified example of usage with
the default linkage strategy, see `hashed_merge_template_minimal.Rmd`.
