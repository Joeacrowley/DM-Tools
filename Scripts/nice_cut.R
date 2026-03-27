nice_cut <-
function (variable, keep_user_na = F, breaks, dig_lab = 1, lab_style = 1) 
{
    if (keep_user_na == F) {
        variable <- user_na_to_na(variable)
    }
    new_variable <- cut(variable, breaks, include.lowest = T, right = T, dig.lab = dig_lab)
    new_levels <- new_variable %>% levels()
    new_levels <- as.character(new_levels)
    if (lab_style == 1) {
        new_levels <- str_replace(new_levels, "\\(|\\[", "Greater than ")
        new_levels <- str_replace(new_levels, ",", " and up to ")
    }
    else if (lab_style == 2) {
        new_levels <- str_replace(new_levels, "\\(|\\[", "")
        new_levels <- str_replace(new_levels, ",", " > & <= ")
    }
    else if (lab_style == 3) {
        new_levels <- str_replace(new_levels, "\\(|\\[", "")
        new_levels <- str_replace(new_levels, ",", "-")
    }
    new_levels <- str_remove(new_levels, "]")
    factor(new_variable, labels = new_levels)
}
