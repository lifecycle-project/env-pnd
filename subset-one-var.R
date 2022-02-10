df = "analysis_df"
var_to_subset = "ndvi300_preg"
band_action = "ge_l"


message("** Step 1 of 9: Checking input data ... ", appendLF = FALSE)  
if (is.null(conns)) {
  conns <- datashield.connections_find()
}

start_objs <- ds.ls(datasources = conns)

available_var <- .checkDataAvailable(
  df = df,
  var = var_to_subset,
  conns = conns) %>%
  dplyr::filter_at(vars(-cohort), all_vars(. == FALSE))

valid_coh <- available_var$cohort


message("** Step 2 of 9: Defining subsets ... ", appendLF = FALSE)

quant_bands <- .getQuantileBands(
  df = df,
  var = var)

op_symbol <- .convertBooleText(band_action)

boole_ref <- .makeBooleRef(
  lower_vals = quant_bands$lower,
  lower_op = op_symbol[1],
  upper_vals = quant_bands$upper,
  upper_op = op_symbol[2]
)

boole_ref %>%
  pmap(function(value_1, op_1, value_2, op_2, boole_short, ...) {
    .BooleTwoConditions(
      df = df,
      var = var_to_subset,
      value_1 = value_1,
      op_1 = op_1,
      value_2 = value_2,
      op_2 = op_2,
      newobj = boole_short,
      conns = conns[valid_coh]
    )
  })

message("** Step 3 of 9: Check for disclosure issues ... ", appendLF = FALSE)

discloure_ref <- boole_ref$boole_short %>%
  map(
    ~ .checkDisclosure(
      bin_vec = .x,
      conns = conns[valid_coh])) %>%
  bind_rows()

if (nrow(discloure_ref) < 1) {
  stop("No subsets can be created as they would all contain fewer rows than the disclosure filter value")
}

failed_disclosure <- discloure_ref %>%
  left_join(., boole_ref, by = "boole_short") %>%
  dplyr::filter(enough_obs == FALSE)

if (nrow(failed_disclosure) > 1) {
  warning(
    "The following subsets cannot be created as they would contain fewer observations
      than the disclosure filter value: \n\n",
    paste0(failed_disclosure$cohort, ": ", failed_disclosure$subset_name, sep = "\n")
  )
}

message("** Step 5 of 9: Creating subsets ... ", appendLF = FALSE)

subset_ref <- left_join(boole_ref, discloure_ref, by = "boole_short") %>%
  dplyr::filter(enough_obs == TRUE)

subset_ref %>%
  pmap(
    function(cohort, boole_short, subset_short, ...) {
      ds.dataFrameSubset(
        df.name = "df_slim",
        V1.name = boole_short,
        V2.name = "1",
        Boolean.operator = "==",
        keep.NAs = TRUE,
        newobj = subset_short,
        datasources = conns[cohort]
      )
    }
  )







.getQuantileBands <- function(df, var){
  
  stats <- dh.getStats(
    df = df,
    vars = var)
  
  quants <- stats$continuous %>% 
    dplyr::filter(cohort == "combined") %>%
    dplyr::select(perc_25, perc_50, perc_75)
  
  out <- tibble(
    lower = c(0, quants) %>% unlist,
    upper = c(quants, 1) %>% unlist
  )
  
  return(out)
  
}

