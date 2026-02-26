#' Colon Cancer Recurrence Data (Example Dataset)
#'
#' A processed subset of the \code{survival::colon} dataset restricted to the
#' recurrence endpoint (\code{etype == 1}), providing one row per patient.
#' Variables have been relabelled with clinically descriptive names and
#' factor levels suitable for direct use in TernTables functions. This dataset
#' is provided as a ready-to-use example for demonstrating \code{ternD()} and
#' \code{ternG()} functionality.
#'
#' @format A tibble with 929 rows and 12 variables:
#' \describe{
#'   \item{ID}{Integer patient identifier.}
#'   \item{Age_Years}{Age at study entry (years).}
#'   \item{Sex}{Patient sex: \code{"Female"} or \code{"Male"}.}
#'   \item{Colonic_Obstruction}{Colonic obstruction present: \code{"N"} or \code{"Y"}.}
#'   \item{Bowel_Perforation}{Bowel perforation present: \code{"N"} or \code{"Y"}.}
#'   \item{Positive_Lymph_Nodes_n}{Number of positive lymph nodes detected.}
#'   \item{>_4_Positive_Nodes}{More than 4 positive lymph nodes: \code{"N"} or \code{"Y"}.}
#'   \item{Tumor_Adherence}{Tumour adherence to surrounding organs: \code{"N"} or \code{"Y"}.}
#'   \item{Tumor_Differentiation}{Tumour differentiation grade: \code{"Well"},
#'     \code{"Moderate"}, or \code{"Poor"}.}
#'   \item{Extent_of_Local_Spread}{Depth of tumour penetration: \code{"Submucosa"},
#'     \code{"Muscle"}, \code{"Serosa"}, or \code{"Contiguous Structures"}.}
#'   \item{Recurrence}{Recurrence status: \code{"No Recurrence"} or \code{"Recurrence"}.}
#'   \item{Treatment_Arm}{Randomised treatment: \code{"Levamisole + 5FU"},
#'     \code{"Levamisole"}, or \code{"Observation"}.}
#' }
#' @source Derived from \code{survival::colon} (Laurie et al., 1989).
#'   See \code{?survival::colon} for full provenance.
#'   Pre-processing script: \code{data-raw/tern_colon.R}.
#' @examples
#' data(tern_colon)
#' head(tern_colon)
"tern_colon"
