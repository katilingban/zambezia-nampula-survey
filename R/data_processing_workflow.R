################################################################################
#
#'
#' Create data processing workflwo using DiagrammeR
#'
#
################################################################################

draw_data_processing_graph <- function() {
  DiagrammeR::mermaid("
    graph TD
    C[cleanstr1.do]                          --> B[final_dataset_processing.do]
    A(VIGH - UNICEF_2019_30_08_14_50.sav)    --> B[final_dataset_processing.do]
    D[qual_control_corrections.do]           --> B[final_dataset_processing.do]
    B[final_dataset_processing.do]           --> E(survey_fin.dta)
    B[final_dataset_processing.do]           --> F(who_anthrofin.csv)
    F(who_anthrofin.csv)                     --> G>WHO ANTHRO ANALYZER]
    G>WHO ANTHRO ANALYZER]                   --> H(who_anthrofin_zscore.csv)
    E(survey_fin.dta)                        --> I>MERGE via final_dataset_processing.do]
    H(who_anthrofin_zscore.csv)              --> I>MERGE via final_dataset_processing.do]
    I>MERGE via final_dataset_processing.do] --> J(survey_plus_who_fin.dta)
  ")
}


draw_stata_workflow_graph <- function() {
  DiagrammeR::mermaid("
    graph TD
    A(Read VIGH - UNICEF_2019_30_08_14_50.sav)       --> B[Remove records with Status == 'Canceled']
    B[Remove records with Status == 'Canceled']      --> C[Remove extraneous, system-generated variables]
    C[Remove extraneous, system-generated variables] --> D[Handling of child-specific identifiers]
    D[Handling of child-specific identifiers]        --> E[Handling of dates data]
    E[Handling of dates data]                        --> F[Variable renaming]
    F[Variable renaming]                             --> G[Handling 'others' and 'specify' variables]
    G[Handling of others and specify variables]      --> H[Handling variables with differing time units]
    H[Handling variables with differing time units]  --> I[Handling of DK and NR]
    I[Handling of DK and NR]                         --> J[Creation of PID and CID]
    J[Creation of PID and CID]                       --> K[Creation of one child per row data]
    K[Creation of one child per row data]            --> L[Handling of multiple response variables]
    L[Handling of multiple response variables]       --> M[Apply qual_control_corrections.do]
    L[Handling of multiple response variables]       --> N[Apply cleanstr1.do]
    M[Apply qual_control_corrections.do]             --> O[Remove records without u5 children]
    N[Apply cleanstr1.do]                            --> O[Remove records without u5 children]
    O[Remove records without u5 children]            --> P[Remove records without u5 children age]
    P[Remove records without u5 children age]        --> Q[Re-create sample stratas]
    Q[Re-create sample stratas]                      --> R[Save as survey_fin.dta]
    R[Save as survey_fin.dta]                        --> S[Create and save who_anthrofin.csv]
    S[Create and save who_anthrofin.csv]             --> T>WHO ANTHRO ANALYZER]
    T>WHO ANTHRO ANALYZER]                           --> U[Create and save who_anthrofin_zscore.csv]
    U[Create and save who_anthrofin_zscore.csv]      --> V[Merge z-scores data with survey_fin.dta]
    V[Merge z-scores data with survey_fin.dta]       --> W[Save as survey_plus_who_fin.dta]
  ")
}
