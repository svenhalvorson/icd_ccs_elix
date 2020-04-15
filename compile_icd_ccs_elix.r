# The goal for this script is to compile icd 9 & 10 codes and then attach
# the more 'research ready' schemas of CCS, Elixhauser, & CCSR
# I had originally programmed this in python and converted it to R because
# of some memory issues with ipython. As a result some of the ways I
# coded this are a bit strange seeming. Lots more of explicit loops.

# Note that some of the 'fixes' I used to compile this dataset were 
# specific to the original files I used. If you're going to update this
# with newer files, I would go through it slowly and check the completeness
# and uniqueness of the joins.

# Set up ------------------------------------------------------------------

# Need these libraries along with here. Open the R project enclosed to have 
# here recognize where the folder is.
library('tidyverse')
library('readxl')
library('haven')
library('here')

# ICD9 diagnoses and procedures:
icd9_cm_p = 'ICD-9-CM-v32-master-descriptions/CMS32_DESC_LONG_SHORT_DX.xlsx'
icd9_pcs_p = 'ICD-9-CM-v32-master-descriptions/CMS32_DESC_LONG_SHORT_SG.xlsx'

# ICD10 diagnoses and procedures
icd10_cm_p = '2020_Code_Descriptions/2020 Code Descriptions/icd10cm_order_2020.txt'
icd10_pcs_p = 'Zip File 5 2020 ICD-10-PCS Order File (Long and Abbreviated Titles)/icd10pcs_order_2020.txt'

# Multi and single level ccs categories
ccs9_cm_multi_p = 'Multi_Level_CCS_2015/ccs_multi_dx_tool_2015.csv'
ccs9_pcs_multi_p = 'Multi_Level_CCS_2015/ccs_multi_pr_tool_2015.csv'
ccs9_cm_single_p = 'Single_Level_CCS_2015/dxlabel 2015.csv'
ccs9_pcs_single_p = 'Single_Level_CCS_2015/prlabel 2014.csv'

ccs10_cm_p = 'ccs_dx_icd10cm_2019_1/ccs_dx_icd10cm_2019_1.csv'
ccs10_pcs_p = 'ccs_pr_icd10pcs_2020_1/ccs_pr_icd10pcs_2020_1.csv'

# CCSR codes:
ccsr10_cm_p = 'v2020_2/$DXCCSR_v2020-2.CSV'

# Elixhausers:
elix9_p = 'comor_icd9_fmt.sas7bdat'
elix10_p = 'comor_icd10_fmt.sas7bdat'

# Complete ICD9 -----------------------------------------------------------


# We will start with our backbone which is the ICD codes beginning with version 9:
icd9_cm = read_excel(
  here(icd9_cm_p)
)
icd9_pcs = read_excel(
  here(icd9_pcs_p)
)

# So we will prefix these with cms to indicate the source
colnames(icd9_cm) = c('icd_code_fmt', 'cms_long_label', 'cms_short_label')
colnames(icd9_pcs) = c('icd_code_fmt', 'cms_long_label', 'cms_short_label')

icd9_cm['icd_type'] = 'cm'
icd9_pcs['icd_type'] = 'pcs'

icd9_complete = icd9_cm %>%
  bind_rows(icd9_pcs)

icd9_complete['icd_ver'] = 9

rm(icd9_cm, icd9_pcs)


# Complete ICD 10 ---------------------------------------------------------


# Next we'll use the 'order files' from cms.gov for the ICD 10 list
# This is because they are more detailed in their descriptions of the codes.
# The order they give is actually pretty helpful

col_sizes = fwf_widths(c(5, 1, 7, 1, 1, 1, 60, 1, 500))

icd10_cm = read_fwf(
  here(icd10_cm_p),
  col_sizes
)

icd10_pcs = read_fwf(
  here(icd10_pcs_p),
  col_sizes
)
# I'm not entirely sure how to avoid all those warnings about my last
# column not being 500 characters wide but doesn't really matter

# nuke blank columns
icd10_cm = icd10_cm[(1:5)*2-1]
icd10_pcs = icd10_pcs[(1:5)*2-1]

icd10_cm['icd_type'] = 'cm'
icd10_pcs['icd_type'] = 'pcs'


icd10_complete = icd10_cm %>%
  bind_rows(icd10_pcs) %>%
  mutate(icd_ver = 10)

colnames(icd10_complete) = c(
  'cms_order',
  'icd_code_fmt',
  'cms_nonheader',
  'cms_short_label',
  'cms_long_label',
  'icd_type',
  'icd_ver'
)

rm(icd10_pcs, icd10_cm)

# Looks like we have a number of duplicated values... can we identify them?
dupes = icd10_complete %>%
  group_by(icd_code_fmt, icd_type) %>%
  filter(n() > 1)

icd_complete = icd10_complete %>%
  bind_rows(icd9_complete)

rm(icd10_complete, icd9_complete)

# reorder since I'm persnickety
reorder = c('icd_code_fmt', 'icd_ver', 'icd_type',
           'cms_order', 'cms_nonheader',
           'cms_short_label', 'cms_long_label')
icd_complete = icd_complete[reorder]


# ICD 9 CCS ---------------------------------------------------------------

# Now we're on to the CCS categories. This is where we'll
# need to use our formatted codes to bind on these categorized ICD9 codes
ccs9_cm = read_csv(
  here(ccs9_cm_multi_p)
)
ccs9_pcs = read_csv(
  here(ccs9_pcs_multi_p)
)
ccs9_cm['icd_type'] = 'cm'
ccs9_pcs['icd_type'] = 'pcs'

# Translating this rename dictionary from python is a bit more annoying:

oldnames = c(
  "'ICD-9-CM CODE'",
  "'CCS LVL 1'",
  "'CCS LVL 1 LABEL'",
  "'CCS LVL 2'",
  "'CCS LVL 2 LABEL'",
  "'CCS LVL 3'",
  "'CCS LVL 3 LABEL'",
  "'CCS LVL 4'",
  "'CCS LVL 4 LABEL'"
)
newnames = c(
  'hcup_icd_code',
  'ccs_code1',
  'ccs_label1',
  'ccs_code2',
  'ccs_label2',
  'ccs_code3',
  'ccs_label3',
  'ccs_code4',
  'ccs_label4'
)

for(i in seq_along(oldnames)){
  colnames(ccs9_cm) = replace(
    colnames(ccs9_cm),
    colnames(ccs9_cm) == oldnames[i],
    newnames[i]
  )
  colnames(ccs9_pcs) = replace(
    colnames(ccs9_pcs),
    colnames(ccs9_pcs) == oldnames[i],
    newnames[i]
  )
}

# Want to make versions of the ICD codes without quotes or spaces for merging:
ccs9 = ccs9_cm %>%
  bind_rows(ccs9_pcs) %>%
  mutate(
    icd_code_fmt = str_remove_all(
      hcup_icd_code,
      "'| "
    ),
    icd_ver = 9
  )

rm(ccs9_cm, ccs9_pcs)


# Now we wanna go through each of the description columns and try to
# extract the single level CCS category from that. Then coalesce them

extracted_cats = ccs9 %>%
  select(
    matches('label')
  ) %>%
  mutate_all(
    str_extract,
    pattern = '\\[.*[0-9].*\\]?'
  ) %>%
  mutate_all(
    str_remove_all,
    '\\[|\\]'
  ) %>%
  mutate_all(
    str_replace_all,
    '\\.',
    ' '
  ) %>%
  mutate_all(
    trimws
  ) %>%
  # So I see some that have two categories but one of these is a misc. group
  # so let's just remove that.
  mutate(
    ccs_code = coalesce(
      ccs_label1,
      ccs_label2,
      ccs_label3,
      ccs_label4
    ),
    ccs_code = str_replace(
      ccs_code,
      '259  and 260',
      '260'
    ),
    ccs_code = trimws(ccs_code),
    ccs_code = paste0(
      "'",
      ccs_code,
      "'"
    )
  )

ccs9['ccs_code'] = extracted_cats['ccs_code']

# The single level CCS descriptions are in other files so let's get those up in here
ccs9_single_cm = read_csv(
  here(ccs9_cm_single_p)
)

ccs9_single_pcs = read_csv(
  here(ccs9_pcs_single_p),
  col_types = cols_only(
    `CCS PROCEDURE CATEGORIES` = 'c',
    `CCS PROCEDURE CATEGORIES LABELS` = 'c'
  )
)



colnames(ccs9_single_cm) = c('ccs_code', 'ccs_label')
colnames(ccs9_single_pcs) = c('ccs_code', 'ccs_label')

ccs9_single_cm['icd_type'] = 'cm'
ccs9_single_pcs['icd_type'] = 'pcs'

ccs9_single = ccs9_single_cm %>%
  bind_rows(ccs9_single_pcs) %>%
  mutate(
    ccs_code = trimws(ccs_code),
    ccs_code = paste0("'", ccs_code, "'")
  )

ccs9 = ccs9 %>%
  left_join(ccs9_single)


# ICD10 CCS ---------------------------------------------------------------


# Same deal but for ICD10. this time the description levels only go 2 deep
# and the single level categories are in the same file
ccs10_cm = read_csv(
  here(ccs10_cm_p)
)
ccs10_pcs = read_csv(
  here(ccs10_pcs_p)
)
ccs10_cm['icd_type'] = 'cm'
ccs10_pcs['icd_type'] = 'pcs'
ccs10_cm['icd_ver'] = 10
ccs10_pcs['icd_ver'] = 10

# Again gonna do this funny way of renaming to re-use my python code...

oldnames = c(
  "'ICD-10-CM CODE'",
  "'ICD-10-PCS CODE'",
  "'CCS CATEGORY'",
  "'ICD-10-CM CODE DESCRIPTION'",
  "'ICD-10-PCS CODE DESCRIPTION'",
  "'CCS CATEGORY DESCRIPTION'",
  "'MULTI CCS LVL 1'",
  "'MULTI CCS LVL 1 LABEL'",
  "'MULTI CCS LVL 2'",
  "'MULTI CCS LVL 2 LABEL'"
)
newnames = c(
  'hcup_icd_code',
  'hcup_icd_code',
  'ccs_code',
  'hcup_icd_label',
  'hcup_icd_label',
  'ccs_label',
  'ccs_code1',
  'ccs_label1',
  'ccs_code2',
  'ccs_label2'
)

for(i in seq_along(oldnames)){
  colnames(ccs10_cm) = replace(
    colnames(ccs10_cm),
    colnames(ccs10_cm) == oldnames[i],
    newnames[i]
  )
  colnames(ccs10_pcs) = replace(
    colnames(ccs10_pcs),
    colnames(ccs10_pcs) == oldnames[i],
    newnames[i]
  )
}

ccs10 = ccs10_cm %>%
  bind_rows(ccs10_pcs) %>%
  mutate(
    icd_code_fmt = str_remove_all(
      hcup_icd_code,
      "'| |\\."
    )
  )


ccs_complete = ccs10 %>%
  bind_rows(ccs9)

rm(ccs10, ccs10_cm, ccs10_pcs, ccs9, extracted_cats)

# I don't know why I feel compelled to re-order so often but I do
reorder = c('icd_code_fmt', 'hcup_icd_code', 'hcup_icd_label',
           'icd_ver', 'icd_type', 'ccs_code',
           'ccs_label', 'ccs_code1', 'ccs_label1',
           'ccs_code2', 'ccs_label2', 'ccs_code3',
           'ccs_label3', 'ccs_code4', 'ccs_label4')
ccs_complete = ccs_complete[reorder]



# Combine CMS & HCUP data -------------------------------------------------

# Now we have the unenviable task of joining this data back to our list
# of all ICD codes.

# Use this for basic checks:
exact_matches = icd_complete %>%
  inner_join(
    ccs_complete,
    by = c('icd_code_fmt', 'icd_ver', 'icd_type')
  )


leftover_ccs = ccs_complete %>%
  anti_join(
    exact_matches,
    by = c('icd_code_fmt', 'icd_ver', 'icd_type')
  )

nrow(leftover_ccs)/nrow(ccs_complete) # about 2% didn't match, not too bad

# Try some experiments. I see some glaucoma diagnoses in the leftover_ccs
# like 	icd_code_fmt H4011X0, H4011X1, H4011X2, H4011X3, H4011X4
glauc = icd_complete %>%
  filter(
    str_detect(icd_code_fmt,
               'H40'
    )
)
glauc2 = leftover_ccs %>%
  filter(
    str_detect(icd_code_fmt,
               'H40'
    )
  )

# So I'm noticing that these seem to have an X in them which I read is a standin
# for some kind of option like laterality or severity
# may have to do something like split the before and after the X

# What about these hepatits codes:	Z225, Z2250, Z2251, Z2252, Z2259
hep = icd_complete %>%
  filter(
    str_detect(
      icd_code_fmt,
      'Z225'
    )
)
# Straight up can't find these in the original file. Googling says these
# were removed...

leftover_icd = icd_complete %>%
  anti_join(
    exact_matches,
    by = c('icd_code_fmt', 'icd_ver', 'icd_type')
  )

nrow(leftover_icd)/nrow(icd_complete)
# 12% didn't match, nto surprising as not every ICD has an associated CCS

# I spent a bunch of time experimenting and came to the conclusion that
# 1) The parts that join do so on a 1-1 basis
# 2) The ICD codes that don't join are almost all headers
# 3) The CCS codes that don't join are usually:
#       a) ICD 9 headers with differences about whether there is a trailing zero
#       b) ICD 10 codes with wildcard Xs in them for laterality/location

# Let's spend a little time trying to get the codes with Xs added on. My strategy
# here will be to split them on the X and try to join on the before and after
# columns.

has_x = leftover_ccs %>%
  filter(
    str_detect(
      icd_code_fmt,
      '.+X'
    )
  ) %>%
  mutate(
    x_len = str_extract(
      icd_code_fmt,
      'X+'
    ),
    x_len = nchar(x_len),
    pre_x = str_extract(
      icd_code_fmt,
      '.+X'
    ),
    pre_x = str_remove_all(
      pre_x,
      'X'
    ),
    post_x = str_extract(
      icd_code_fmt,
      'X.+'
    ),
    post_x = str_remove_all(
      post_x,
      'X'
    ),
    pre_len = nchar(pre_x),
    post_len = nchar(post_x)
  ) %>%
  mutate_at(
    vars(matches('pre_|post_')),
    replace_na,
    0
  )


# Alright that's pretty nice. Now let's take all those unmatched icd codes and
# split them at the combinations of pre_len and post_len
split_table = has_x %>%
  select(matches('_len')) %>%
  distinct()

for(i in 1:nrow(split_table)){
  leftover_icd[paste0('pre',i)] = substr(
    x = leftover_icd$icd_code_fmt,
    start = 0,
    stop = split_table$pre_len[i]
  )

  leftover_icd[paste0('post',i)] = substring(
    text = leftover_icd$icd_code_fmt,
    first = split_table$pre_len[i] + split_table$x_len[i] + 1
  )

  # Python's string multiplication was really nice here. I need to do this to get
  # the list of x's to place betweenthe start and end of the code:
  xs = paste0(
      rep(
        'X',
        times = split_table$x_len[i]
      ),
      collapse = ''
    )

  leftover_icd[paste0('new',i)] = paste0(
    leftover_icd[[paste0('pre',i)]],
    xs,
    leftover_icd[[paste0('post',i)]]
  )

  # I think I want to null out spots that didn't meet the required lengths since
  # They might accidentally join on to something else:
  pre_lens = nchar(leftover_icd[[paste0('pre',i)]])
  post_lens = nchar(leftover_icd[[paste0('post',i)]])
  kicks = (pre_lens != split_table$pre_len[i]) | (post_lens != split_table$post_len[i])
  leftover_icd[kicks, paste0('new',i)] = NA

}
rm(pre_lens, post_lens, kicks, xs)

# Now join in a loop:
str_join = function(i, left, right){

  join_con = 'icd_code_fmt'
  names(join_con) = paste0('new', i)

  left %>%
    inner_join(
      right,
      by = join_con

    )
}

mapped_xs = lapply(
  X = 1:nrow(split_table),
  FUN = str_join,
  left = leftover_icd,
  right = has_x
) %>%
  bind_rows()

# wao much genius. All that for another 66 records!!!!

# Anyways... I guess we'll move on. To incorporate this I think
# I will just join this new one on the CCS categories and coalesce

mapped_xs = mapped_xs %>%
  select(icd_code_fmt, hcup_icd_code) %>%
  distinct() %>%
  rename(icd_code_fmt2 = icd_code_fmt)

# Now join. Notice that this does increase the number of rows but that is
# because we want S028XXA to be associated with both S02831A and S02832A

ccs_complete = ccs_complete %>%
  left_join(mapped_xs) %>%
  mutate(icd_code_fmt = coalesce(
    icd_code_fmt2,
    icd_code_fmt
    )
  ) %>%
  select(-icd_code_fmt2)


# So as a result, I think I'm just going to do a full join and note the sources

icd_ccs = icd_complete %>%
  full_join(
    ccs_complete,
    by = c('icd_code_fmt', 'icd_ver', 'icd_type')
  ) %>%
  mutate(
    cms_present = !is.na(cms_short_label),
    hcup_present = !is.na(hcup_icd_code)
  )

# Another check for duplicates:
icd_ccs %>%
  group_by(icd_code_fmt, icd_type, icd_ver) %>%
  filter(n() > 1)
# wanderful
rm(icd_complete, ccs_complete, exact_matches, leftover_icd, leftover_ccs)


# Add CCSR ----------------------------------------------------------------

# Now that the CCSR has been released for diagnoses, we should add these in as well.
ccsr10_cm = read_csv(
  here(ccsr10_cm_p),
  col_types = cols(.default = "c")
)

# Same rename, format, add cols:
rename_dict3 = c(
  "'ICD-10-CM CODE'" = 'hcup_icd_code',
  "'ICD-10-CM CODE DESCRIPTION'" = 'ccsr_icd_code_label',
  "'Default CCSR CATEGORY" = 'ccsr_code_default',
  "'Default CCSR CATEGORY DESCRIPTION'" = 'ccsr_default',
  "'CCSR CATEGORY 1'" = 'ccsr_code1',
  "'CCSR CATEGORY 1 DESCRIPTION'" = 'ccsr_label1',
  "'CCSR CATEGORY 2'" = 'ccsr_code2',
  "'CCSR CATEGORY 2 DESCRIPTION'" = 'ccsr_label2',
  "'CCSR CATEGORY 3'" = 'ccsr_code3',
  "'CCSR CATEGORY 3 DESCRIPTION'" = 'ccsr_label3',
  "'CCSR CATEGORY 4'" = 'ccsr_code4',
  "'CCSR CATEGORY 4 DESCRIPTION'" = 'ccsr_label4',
  "'CCSR CATEGORY 5'" = 'ccsr_code5',
  "'CCSR CATEGORY 5 DESCRIPTION'" = 'ccsr_label5'
)

for(i in seq_along(rename_dict3)){

  colnames(ccsr10_cm) = replace(
    colnames(ccsr10_cm),
    colnames(ccsr10_cm) == names(rename_dict3)[i],
    rename_dict3[i]
  )

}

ccsr10_cm = ccsr10_cm %>%
  mutate(
    icd_ver = 10,
    icd_type = 'cm',
    icd_code_fmt = str_remove_all(
      hcup_icd_code,
      " |'"
    )
  )

# Seems like we can just join here and check the leftovers
icd_ccs = icd_ccs %>%
  left_join(
    ccsr10_cm,
    by = c('icd_code_fmt', 'hcup_icd_code', 'icd_ver', 'icd_type')
  )

# Did we miss any?
ccsr_leftovers = ccsr10_cm %>%
  anti_join(
    icd_ccs,
    by = 'icd_code_fmt'
  )

# just a couple records so let's just see what the potential candidates are for
# these injury codes:
patterns = ccsr_leftovers %>%
  pluck('icd_code_fmt') %>%
  paste0(collapse = '|') %>%
  str_replace_all('X', '.')

injur_codes = icd_ccs %>%
  filter(
    icd_ver == 10,
    icd_type == 'cm',
    grepl(
      pattern = patterns,
      x = icd_code_fmt
    )
  )

# these all look reasonable
patterns = ccsr_leftovers %>%
  pluck('icd_code_fmt') %>%
  str_replace_all('X', '.')
patterns = paste0('^', patterns, '$')

for(i in seq_along(patterns)){

  replace_indices = icd_ccs$icd_ver == 10 &
    icd_ccs$icd_type == 'cm' &
    grepl(
      pattern = patterns[i],
      x = icd_ccs$icd_code_fmt
    ) &
    is.na(icd_ccs$ccsr_code1)

  icd_ccs[replace_indices, 'ccsr_code1'] = ccsr_leftovers$ccsr_code1[i]
  icd_ccs[replace_indices, 'ccsr_label1'] = ccsr_leftovers$ccsr_label1[i]
}


# Add Elixhauser ----------------------------------------------------------

# So the format of these files is that we have ranges of codes
elix9 = read_sas(
  here(elix9_p)
)
elix10 = read_sas(
  here(elix10_p)
)

elix = elix10 %>%
  mutate(icd_ver = 10) %>%
  bind_rows(elix9) %>%
  mutate(
    icd_ver = replace_na(icd_ver, 9),
    icd_type = 'cm'
  ) %>%
  select(icd_ver, icd_type, START, END, LABEL)

# So we end up with a problem here where if we join everything,
# the result is far too large in terms of memory. So I think what I'll do
# is join off the first digit of the codes as well.
# Veryify that the first and last of the start and end are all equal:
elix %>%
  summarize(
    sum(substr(START, 1, 1) != substr(END, 1, 1))
  )
# oh my there is one such row.
elix = elix %>%
  filter(
    !LABEL %in% c('', 'NONE'),
    substr(START, 1, 1) == substr(END, 1, 1)
  ) %>%
  add_row(
    icd_ver = 9,
    icd_type = 'cm',
    START = '4950',
    END = '49999',
    LABEL = 'CHRNLUNG'
  ) %>%
  add_row(
    icd_ver = 9,
    icd_type = 'cm',
    START = '500',
    END = '505',
    LABEL = 'CHRNLUNG'
  ) %>%
  mutate(
    firstchar = substr(START, 1, 1)
  )

elix_match = icd_ccs %>%
  filter(icd_type == 'cm') %>%
  select(icd_code_fmt, cms_long_label, ccs_label, hcup_icd_label, icd_ver) %>%
  mutate(
    firstchar = substr(icd_code_fmt, 1, 1)
  ) %>%
  full_join(elix) %>%
  filter(
    icd_code_fmt >= START,
    icd_code_fmt <= END
  )

# Alright, started with an eyeball test to see how well the matching went and it
# looks good. Questions now are:
# 1) Any ICD codes match to more than one category set.
# 2) Any category set * icd_ver not match to anything
# 3) Any category set not have both icd versions

# 1)
elix_match %>%
  group_by(icd_code_fmt, icd_ver) %>%
  filter(n() > 1)

# 2)
elix %>%
  anti_join(
    elix_match,
    by = c("START", "END", "LABEL")
  )

# 3)
elix %>%
  group_by(LABEL) %>%
  filter(n_distinct(icd_ver) < 2)

# Woot newt

# Okay join this back on and we're done!
elix_match = elix_match %>%
  select(icd_code_fmt, icd_ver, LABEL) %>%
  rename(elix_comorb = LABEL)

icd_ccs = icd_ccs %>%
  left_join(elix_match)

# Export ------------------------------------------------------------------


# alright, I did a little check to see that there are no tabs in the file
# so we should be good to write as a tsv:

icd_ccs %>%
  select(
    matches('^icd_'),
    matches('^cms_'),
    matches('^hcup_'),
    matches('^ccs_'),
    matches('^ccsr_'),
    elix_comorb
  ) %>% 
  write_tsv(
    here(paste0('icd_ccs_elix_', str_remove_all(Sys.Date(), '-'), '.txt'))
  )

