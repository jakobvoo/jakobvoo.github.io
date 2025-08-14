library(tidyverse)
library(ggtext)
library(openxlsx)
library(skimr)
library(janitor)
library(strayr)


#* Theme Parameters --------------------------------------------------------------------------------
theme_update(

    # Styling for Plot Area ........................................................................
    plot.margin           = margin(t = 20, r = 20, b = 20, l = 20),
    plot.background       = element_rect(fill = "grey98", colour = "grey98"),
    plot.title            = element_text(
        colour  = "grey10",
        size    = 26,
        face    = "plain",
        margin  = margin(t = 15)
    ),
    plot.subtitle         = element_markdown(
        colour     = "grey30",
        size       = 16,
        lineheight = 1.35,
        margin     = margin(t = 15, b = 40)
    ),
    plot.caption          = element_text(
        colour     = "grey30",
        size       = 12,
        lineheight = 1.2,
        hjust      = 0,
        margin     = margin(t = 10)
    ),
    plot.title.position   = "plot",
    plot.caption.position = "plot",

    # Styling for Axes .............................................................................
    axis.title            = element_text(size = 18, margin = margin(t = 5)),
    axis.text             = element_text(colour = "grey40"),
    axis.text.x           = element_text(size = 18, margin = margin(t = 5)),
    axis.text.y           = element_text(size = 15, margin = margin(l = 5, r = 5)),
    axis.ticks            = element_line(colour = "grey98", linewidth = 0.5),
    axis.ticks.length.x   = unit(0.3, "lines"),
    axis.ticks.length.y   = unit(0, "lines"),

    # Styling for Grid & Panel .....................................................................
    panel.grid.major      = element_line(colour = "grey90", linewidth = 0.2),
    panel.grid.minor      = element_line(colour = "grey90", linewidth = 0.1),
    panel.background      = element_rect(fill = "grey98", colour = "grey98"),

    # Styling for Legend ...........................................................................
    legend.background     = element_rect(fill = "grey98", colour = "grey98"),
    legend.title          = element_text(size = 15, margin = margin(l = 5, r = 5)),
    legend.text           = element_text(size = 12, margin = margin(t = 5, l = 5, r = 5)),
    legend.position       = "none"

)



##### URLs
url___abs_osca2024_to_anzsco2019 <- "https://www.abs.gov.au/statistics/classifications/osca-occupation-standard-classification-australia/2024-version-1-0/data-downloads/OSCA%20correspondence%20tables%20v2.xlsx"
url___jsa_occupational_gender_pay_gap <- "https://www.jobsandskills.gov.au/sites/default/files/2025-08/occupational_gender_pay_gap.xlsx"



# Dataset 1
ABS_OSCA2024_TO_ANZSCO2019 <- url___abs_osca2024_to_anzsco2019 |>
    read.xlsx(sheet = "Table 2", rows = 6:1388, colNames = FALSE) |>
    as_tibble() |>
    transmute(
        osca_occupation_code   = as.character(X1),
        osca_occupation        = X2,
        anzsco_occupation_code = as.character(X3),
        is_partial_matched     = X4 == "p",
        anzsco_occupation      = X5
    ) |>
    fill(starts_with("osca_"), .direction = "down") |>
    arrange(osca_occupation_code, anzsco_occupation_code)

# Dataset 2
JSA_OCCUPATIONAL_GENDER_PAY_GAP <- url___jsa_occupational_gender_pay_gap |>
    read.xlsx(colNames = TRUE) |>
    as_tibble() |>
    clean_names() |>
    mutate(across(ends_with("median"), ~ na_if(.x, "-") |> as.numeric())) |>
    mutate(code = as.character(code)) |>  # 2019 version upon close investigation...
    rename(jsa_anzsco_occupation_code = code, jsa_occupation = occupation_anzsco_6_digit) |>
    arrange(jsa_anzsco_occupation_code, cohort_filter)

# Combined
OCCUPATIONAL_GENDER_PAY_GAP <- ABS_OSCA2024_TO_ANZSCO2019 |>

    # Join 1
    full_join(osca2024, by = join_by(osca_occupation_code, osca_occupation)) |>
    select(-skill_level) |> # Not complete and also not super useful for this analysis

    # Join 2
    expand_grid(cohort_filter = unique(JSA_OCCUPATIONAL_GENDER_PAY_GAP$cohort_filter)) |>
    full_join(
        JSA_OCCUPATIONAL_GENDER_PAY_GAP,
        by = join_by(anzsco_occupation_code == jsa_anzsco_occupation_code, cohort_filter)
    ) |>

    #
    mutate(
        gender_segregation_intensity = gender_segregation_intensity |> factor(
            levels = c(
                "Highly female dominated",
                "Almost completely female dominated",
                "Moderately female dominated",
                "Gender balanced",
                "Moderately male dominated",
                "Almost completely male dominated",
                "Highly male dominated"
            )
        ),
        .gender_segregation_intensity_code = case_when(
            gender_segregation_intensity == "Highly female dominated"             ~ -7,
            gender_segregation_intensity == "Almost completely female dominated"  ~ -5,
            gender_segregation_intensity == "Moderately female dominated"         ~ -3,
            gender_segregation_intensity == "Gender balanced"                     ~ 0,
            gender_segregation_intensity == "Moderately male dominated"           ~ 3,
            gender_segregation_intensity == "Almost completely male dominated"    ~ 5,
            gender_segregation_intensity == "Highly male dominated"               ~ 7,
            .default                                                              = NA
        ),
        .gender_segregation = case_when(
            sign(.gender_segregation_intensity_code) == -1 ~ "Female",
            sign(.gender_segregation_intensity_code) == 1  ~ "Male",
            sign(.gender_segregation_intensity_code) == 0  ~ "Balanced",
            .default                                      = NA
        ),
        .health_profession_group = case_when(
            osca_minor_code %in% 264:265       ~ osca_minor,
            osca_minor_code %in% 261:263       ~ "Allied Health Professionals",
            osca_minor_code == 266             ~ "Other Health Professionals",
            .default                           = NA
        )
    ) |>
    arrange(osca_occupation_code, anzsco_occupation_code, cohort_filter) |>
    mutate(across(c(osca_major, osca_submajor, osca_minor, osca_unit), fct_inorder))



# Figure 1
## v1 - good to highlight "shape"
OCCUPATIONAL_GENDER_PAY_GAP |>
    filter(cohort_filter == "Whole workforce") |>
    ggplot(aes(x = gender_pay_gap_percent_median, y = osca_major)) +
    geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_violin() +
    stat_summary(fun = mean, geom = "point", color = "red", size = 4)

# ## v2 - good to highlight "outliers" & Q1 & Q3
# OCCUPATIONAL_GENDER_PAY_GAP |>
#     filter(cohort_filter == "Whole workforce") |>
#     ggplot(aes(x = gender_pay_gap_percent_median, y = osca_major)) +
#     geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
#     geom_boxplot() +
#     stat_summary(fun = mean, geom = "point", color = "red", size = 4)

# Figure 2
OCCUPATIONAL_GENDER_PAY_GAP |>
    filter(cohort_filter == "Whole workforce", osca_major_code == 2) |>
    ggplot(aes(x = gender_pay_gap_percent_median, y = osca_submajor)) +
    geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_violin() +
    stat_summary(fun = mean, geom = "point", color = "red", size = 4)

# Figure 3
OCCUPATIONAL_GENDER_PAY_GAP |>
    filter(cohort_filter == "Whole workforce", osca_submajor_code == 26) |>
    ggplot(aes(x = gender_pay_gap_percent_median, y = osca_minor, fill = .health_profession_group)) +
    geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_violin(alpha = 0.3) +
    stat_summary(aes(colour = .health_profession_group), fun = mean, geom = "point", size = 4) +
    theme(legend.position = "bottom")

# Figure 3.1 - Too many things going on
OCCUPATIONAL_GENDER_PAY_GAP |>
    filter(cohort_filter == "Whole workforce", osca_submajor_code == 26) |>
    filter(!is.na(gender_pay_gap_percent_median)) |> ### TODO - add them back
    ggplot(
        aes(
            x = male_annual_income_median,
            y = female_annual_income_median,
            colour = .health_profession_group,
        )
    ) +
    # geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(
        aes(
            # size = abs(.gender_segregation_intensity_code) + 2,
            shape = .gender_segregation
        ),
        alpha = 0.5, size = 5
    ) +
    scale_size_identity() +
    scale_x_continuous(labels = scales::label_currency()) +
    scale_y_continuous(labels = scales::label_currency()) +
    theme(legend.position = "bottom")


### Functionalise F4-F6
# Figure 4
OCCUPATIONAL_GENDER_PAY_GAP |>
    mutate(.segregation_intensity = abs(.gender_segregation_intensity_code)) |>
    filter(cohort_filter == "Whole workforce", osca_minor_code == 264) |>
    filter(!is.na(gender_pay_gap_percent_median)) |> ### TODO - add them back
    ggplot(aes(x = gender_pay_gap_percent_median, y = fct_reorder(osca_occupation, gender_pay_gap_percent_median))) +
    geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_point(aes(shape = .gender_segregation), size = 4, alpha = 0.5) +
    theme(legend.position = "bottom") +
    facet_grid(~.segregation_intensity)


# Figure 5
OCCUPATIONAL_GENDER_PAY_GAP |>
    mutate(.segregation_intensity = abs(.gender_segregation_intensity_code)) |>
    filter(cohort_filter == "Whole workforce", osca_minor_code == 265) |>
    filter(!is.na(gender_pay_gap_percent_median)) |> ### TODO - add them back
    ggplot(aes(x = gender_pay_gap_percent_median, y = fct_reorder(osca_occupation, gender_pay_gap_percent_median))) +
    geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_point(aes(shape = .gender_segregation), size = 4, alpha = 0.5) +
    theme(legend.position = "bottom") +
    facet_grid(~.segregation_intensity)

# Figure 6
OCCUPATIONAL_GENDER_PAY_GAP |>
    mutate(.segregation_intensity = abs(.gender_segregation_intensity_code)) |>
    filter(cohort_filter == "Whole workforce", osca_submajor_code == 26, !(osca_minor_code %in% 264:265)) |>
    filter(!is.na(gender_pay_gap_percent_median)) |> ### TODO - add them back
    ggplot(aes(x = gender_pay_gap_percent_median, y = fct_reorder(osca_occupation, gender_pay_gap_percent_median))) +
    geom_rect(xmin = -0.05, xmax = 0.05, ymin = -Inf, ymax = Inf, fill = "grey90") +
    geom_point(aes(shape = .gender_segregation), size = 4, alpha = 0.5) +
    theme(legend.position = "bottom") +
    facet_grid(~.segregation_intensity)


###

# Option 1
expand.grid(
    f = seq(from = 0, to = 5e5, by = 1e3),
    m = seq(from = 0, to = 5e5, by = 1e3)
) |>
    mutate(pay_gap = (m - f) / m) |>
    filter(is.finite(pay_gap)) |> # Remove for when the calculation doesn't work, i.e., m = 0
    ggplot(aes(x = m, y = f, z = pay_gap)) +
    geom_contour_filled(
        aes(fill = after_stat(level_mid)),
        breaks = seq(from = -1.1, to = 1.1, by = 0.2)
    ) +
    geom_abline(slope = 1, intercept = 0, linewidth = 2) +
    scale_x_continuous(labels = scales::label_currency()) +
    scale_y_continuous(labels = scales::label_currency()) +
    scale_fill_stepsn(
        colours = RColorBrewer::brewer.pal(n = 11, name = "PuOr"),
        breaks = seq(from = -1.1, to = 1.1, by = 0.2)
    ) +
    coord_cartesian(expand = FALSE) +
    theme(legend.position = "bottom")

# Option 2
expand.grid(
    f = seq(from = 0, to = 5e5, by = 1e3),
    m = seq(from = 0, to = 5e5, by = 1e3)
) |>
    mutate(pay_gap = (m - f) / m) |>
    filter(is.finite(pay_gap) & pay_gap > -1) |> # Remove for when the calculation doesn't work, i.e., m = 0
    ggplot(aes(x = m, y = f, fill = pay_gap)) +
    geom_raster() +
    geom_abline(slope = 1, intercept = 0) +
    scale_x_continuous(labels = scales::label_currency()) +
    scale_y_continuous(labels = scales::label_currency()) +
    scale_fill_distiller(palette = "PuOr", direction = -1, ) +
    coord_cartesian(expand = FALSE) +
    theme(legend.position = "bottom")






