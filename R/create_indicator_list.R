################################################################################
#
#'
#' Create indicator sets
#'
#
################################################################################

list_indicator_sets <- function() {
  c(
    "Demographics - respondent", "Demographics - child", "Demographics - spouse",
    "Household income", "Household structure", "Household amenities",
    "Household decision making", "Household membership in community groups",
    "Household mode of travel", "Water, sanitation, and hygiene",
    "Women's decision making", "Women's mental health and alcohol consumption", 
    "Pregnancy characteristics", "Pre- and post-natal care",
    "Prevention of mother-to-child-transmission", "Family planning",
    "Women's Dietary Diversity Score", "Minimum Dietary Diversity for Women",
    "Women's anthropometry", "Child development",
    "Period prevalence of childhood illnesses", 
    "Childhood immunisation", "Vitamin A supplementation", "Deworming",
    "Breastfeeding practices", "Infant and young child feeding",
    "Child anthropometry"
  )
}

################################################################################
#
#'
#' Create indicator list
#'
#'   indicator_set
#'   indicator
#'   indicator_code
#'   indicator_type - proportion or mean
#'   indicator_population - households, WRA, pregnant, 
#'     children 6-59, children 0-5, children 6-23, 
#'     children 12-59, children 12-23
#'
#
################################################################################

create_indicator_list <- function(indicator_set,
                                  indicator,
                                  indicator_code,
                                  indicator_type,
                                  indicator_population) {
  
}



