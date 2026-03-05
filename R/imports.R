#' @import dplyr
#' @import tibble
#' @importFrom rlang .data
#' @importFrom cli cli_rule cli_alert_info cli_alert_success cli_bullets cli_alert_warning
#' @importFrom flextable flextable set_table_properties autofit width font fontsize bold italic bg align border_remove border body_add_flextable compose as_paragraph as_chunk style padding height merge_at add_footer_lines hrule hline_top hline_bottom
#' @importFrom officer read_docx body_add_par body_add_fpar body_add_break body_add_docx fpar ftext fp_text fp_par fp_border body_set_default_section prop_section block_list
#' @importFrom stats chisq.test median na.omit p.adjust quantile sd setNames
#' @importFrom utils globalVariables
#' @importFrom magrittr %>%
#' @importFrom stringr str_extract str_trim str_to_lower str_to_title
NULL

utils::globalVariables(c(".", "test"))