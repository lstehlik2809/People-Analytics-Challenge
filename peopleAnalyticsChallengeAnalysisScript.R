# Uploading libraries
library(tidyverse) # for data manipulation and visualization
library(readxl) # for reading data in the .xlsx format
library(skimr) # for data exploration
library(ggdist) # for data visualization

# Getting sheet names
path <- "./Summer People Analytics Challenge Dataset.xlsx"
readxl::excel_sheets(path = path)

# Uploading data for Challenge #2 (Plan for High Growth)
dataApplications <- readxl::read_xlsx("./Summer People Analytics Challenge Dataset.xlsx", sheet = "Applications", na = "N/A")

# Exploring data
dplyr::glimpse(dataApplications)
skimr::skim(dataApplications)

# Overview of key business questions to be answered using data provided
# 1 How much time do we need to make a hire?
# 2 How many hires do we need per week across our company to be on top of our total workforce growth goal?
# 3 How many candidates do we need to source and screen on a weekly basis to get to this goal?
# 4 Which sources should we focus on? Which sources proved to be the most efficient in the past?
# 5 Where are our hiring bottlenecks? Do we have issues with the top, middle, or bottom of the recruitment funnel?
# 6 Which source should we choose as the primary source of candidates and increase our investment in it to achieve high growth? Which of the hiring sources are the most cost-effective?


# 1 How much time do we need to make a hire? ----

# metric computation
timeToHire <- dataApplications %>%
  dplyr::filter(`Hired?` == "Yes") %>%
  dplyr::mutate(
    timeToHire = `Date Hired` - `Date Applied`
  )

# summary statistics
timeToHire %>%
  dplyr::summarise(
    mean = mean(timeToHire), 
    Q1 = quantile(timeToHire, probs = 0.25),
    median = median(timeToHire),
    Q3 = quantile(timeToHire, probs = 0.75),
    min = min(timeToHire),
    max = max(timeToHire)
  )

# graph
ggplot(timeToHire, aes(1, timeToHire)) + 
  ggdist::stat_halfeye(adjust = .5, width = .7, .width = 0, justification = -.2, point_colour = NA, fill = "grey") + 
  ggplot2::geom_boxplot(width = .2, outlier.shape = NA) + 
  ggplot2::geom_jitter(width = .05, color = "grey", alpha = 1) +
  ggplot2::stat_summary(fun=mean, geom="point", shape=18, size=5, color="#541388", fill="#541388") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(breaks = seq(20,100,10)) +
  ggplot2::labs(
    y = "Days to hire",
    x = ""
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.ticks.y = element_blank(),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )



# 2 How many hires do we need per week across our company to be on top of our total workforce growth goal? ----

(hiresPerWeek <- ceiling(750/52))


# 3 How many candidates do we need to source and screen on a weekly basis to get to this goal? ----

# computing proportion of candidates being hired
hireRatio <- dataApplications %>%
  #filter(Source %in% c("Recommendation", "Linkedin")) %>%
  dplyr::summarise(
    nApplicants = n(),
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>%
  dplyr::mutate(
    hireRatio = nHired/nApplicants
  ) %>%
  dplyr::pull(hireRatio)


(sourcedScreenedPerWeek <- ceiling(hiresPerWeek/(hireRatio*100)*100))


# 4 Which sources should we focus on? Which sources proved to be the most efficient in the past? ----

# from the perspective of number of applicants
(
nApplicants <- dataApplications %>%
  dplyr::group_by(
    Source
  ) %>%
  dplyr::summarise(
    nApplicants = n()
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(nApplicants)) %>%
  dplyr::mutate(
    nApplicantsAll = sum(nApplicants),
    propApplicants = nApplicants/nApplicantsAll
  )
)

nApplicants %>%
  ggplot2::ggplot(aes(x = forcats::fct_reorder(Source, propApplicants), y = propApplicants)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    x = "",
    y = "Proportion of applicants",
    title = "WHAT BRINGS IN THE MOST APPLICANTS?"
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.line.y = element_line(colour = "#E0E1E6")
  )


# from the perspective of quality of candidates (proportion of candidates being offered a job)
(
candidateQuality <- dataApplications %>%
  dplyr::group_by(
    Source
  ) %>%
  dplyr::summarise(
    nApplicants = n(),
    nOfferedJob = sum(`Job Offered?` == "Yes", na.rm = T),
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    propOfferedJob = nOfferedJob/nApplicants
  ) %>%
  dplyr::arrange(desc(propOfferedJob))
)

candidateQuality %>%
  ggplot2::ggplot(aes(x = forcats::fct_reorder(Source, propOfferedJob), y = propOfferedJob)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    x = "",
    y = "Percentage of applicants offered a job",
    title = "WHAT BRINGS IN THE BEST QUALITY APPLICANTS?"
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.line.y = element_line(colour = "#E0E1E6")
  )

# from the perspective of conversion ratio
(
conversionRatio <- dataApplications %>%
  dplyr::group_by(
    Source
  ) %>%
  dplyr::summarise(
    nApplicants = n(),
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    hireRatio = nHired/nApplicants
  ) %>%
  dplyr::arrange(desc(hireRatio))
)

conversionRatio %>%
  ggplot2::ggplot(aes(x = forcats::fct_reorder(Source, hireRatio), y = hireRatio)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    x = "",
    y = "Proportion of hired applicants",
    title = "WHAT HAS THE BEST CONVERSION RATIO?"
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.line.y = element_line(colour = "#E0E1E6")
  )

# from the perspective of number of hired people
(
nHiredEmpls <- dataApplications %>%
  dplyr::group_by(
    Source
  ) %>%
  dplyr::summarise(
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(nHired)) %>%
  dplyr::mutate(
    nHiredAll = sum(nHired),
    propHired = nHired/nHiredAll
  )
)

nHiredEmpls %>%
  ggplot2::ggplot(aes(x = forcats::fct_reorder(Source, propHired), y = propHired)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::labs(
    x = "",
    y = "Proportion of hired employees",
    title = "WHAT BRINGS IN THE MOST HIRES?"
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.line.y = element_line(colour = "#E0E1E6")
  )


# from the perspective of days to hire
(
hiringSpeed <- timeToHire %>%
  dplyr::group_by(
    Source
  ) %>%
  dplyr::summarise(
    mean = mean(timeToHire), 
    Q1 = quantile(timeToHire, probs = 0.25),
    median = median(timeToHire),
    Q3 = quantile(timeToHire, probs = 0.75),
    min = min(timeToHire),
    max = max(timeToHire)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(mean)
)

hiringSpeed %>%
  ggplot2::ggplot(aes(x = forcats::fct_reorder(Source, -mean), y = mean)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = "",
    y = "Average time to hire (days)",
    title = "WHAT'S THE FASTEST?"
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.line.y = element_line(colour = "#E0E1E6")
  )



# 5 Where are our hiring bottlenecks? Do we have issues with the top, middle, or bottom of the recruitment funnel? ----

# basic statistics for individual phases of the recruitment funnel
(
hiringFunnel <- dataApplications %>%
  dplyr::summarise(
    nApplicants = n(),
    nOfferedJob = sum(`Job Offered?` == "Yes", na.rm = T),
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>% 
  dplyr::mutate(
    firstFilter = nOfferedJob/nApplicants,
    secondFilter = nHired/nOfferedJob
  )
)

# segmented by source
dataApplications %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    nApplicants = n(),
    nOfferedJob = sum(`Job Offered?` == "Yes", na.rm = T),
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    firstFilter = nOfferedJob/nApplicants,
    secondFilter = nHired/nOfferedJob
  )

# graph with the acceptance rate
acceptanceRate <- data.frame(
  category = c("Accepted offers", "Rejected offers"),
  proportion = c(hiringFunnel$secondFilter, 1-hiringFunnel$secondFilter)
) %>%
  dplyr::arrange(desc(proportion)) %>%
  dplyr::mutate(ypos = cumsum(proportion)- 0.5*proportion )

acceptanceRate %>%
  ggplot2::ggplot(aes(x="", y=proportion, fill=fct_reorder(category, proportion))) +
  ggplot2::geom_bar(stat="identity", width=1) +
  ggplot2::coord_polar("y", start=0) +
  ggplot2::geom_text(aes(y = ypos, label = stringr::str_glue("{category}\n({round(proportion,2)*100}%)")), color = "white", size=5, fontface = "bold") +
  ggplot2::scale_fill_manual(values = c("Rejected offers" = "#ffd400", "Accepted offers" = "#541388")) +
  ggplot2::theme_void() +
  ggplot2::theme(legend.position="none") 

dataApplications %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    nApplicants = n(),
    nOfferedJob = sum(`Job Offered?` == "Yes", na.rm = T),
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(
    firstFilter = nOfferedJob/nApplicants,
    secondFilter = nHired/nOfferedJob
  )


# 6 Which source should we choose as the primary source of candidates and increase our investment in it to achieve high growth? Which of the hiring sources are the most cost-effective? ----

# creating a dataset with information about the costs of individual phases of recruitment for individual hiring sources 
# selection costs & reward costs
dataApplicationsEnriched <- dataApplications %>%
  dplyr:: mutate(
    selection_costs = case_when(
      `Job Offered?` == "Yes" ~ 200,
      TRUE ~ 80
    ),
    reward_costs = case_when(
      Source == "Recommendation" & `Hired?` == "Yes" ~ 1000,
      TRUE ~ 0
    )
  )

# sourcing costs
allCosts <- dataApplicationsEnriched %>%
  dplyr::group_by(Source) %>%
  dplyr::summarise(
    selection_costs = sum(selection_costs),
    reward_costs = sum(reward_costs)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    sourcing_costs = case_when(
      Source == "Recommendation" ~ 3000,
      Source == "Instagram" ~ 9000,
      Source == "Indeed" ~ 9000,
      Source == "Linkedin" ~ 9000,
      Source == "Other" ~ 3000,
      Source == "Glassdoor" ~ 6000,
      TRUE ~ 0
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    overall_costs = sum(selection_costs, reward_costs, sourcing_costs)
  ) %>%
  dplyr::ungroup()

# number of new hires
nHired <- dataApplications %>%
  dplyr::group_by(
    Source
  ) %>%
  dplyr::summarise(
    nHired = sum(`Hired?` == "Yes", na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(desc(nHired))

# overall cost per hire
allCosts %>%
  dplyr::left_join(nHired, by = "Source") %>%
  dplyr::mutate(
    costPerHire = overall_costs/nHired
  ) %>%
  dplyr::arrange(costPerHire) %>%
  dplyr::summarise(
    overall_costs = sum(overall_costs),
    nHired = sum(nHired),
    costPerHire = overall_costs / nHired
  )

# overall cost per hire by sources
(
  costPerHire <- allCosts %>%
    dplyr::left_join(nHired, by = "Source") %>%
    dplyr::mutate(
      costPerHire = overall_costs/nHired
    ) %>%
    dplyr::arrange(costPerHire)
)


# graph
costPerHire %>%
  ggplot2::ggplot(aes(x = forcats::fct_reorder(Source, -costPerHire), y = costPerHire)) +
  ggplot2::geom_bar(stat = "identity", fill = "grey") +
  ggplot2::coord_flip() +
  ggplot2::scale_y_continuous(
    labels = scales::number_format(prefix = "$", suffix = "K", scale = 0.001, accuracy = 1),
    limits = c(0,12500)) +
  ggplot2::labs(
    x = "",
    y = "Cost per hire",
    title = "WHAT IS THE MOST COST-EFFECTIVE?"
  ) +
  ggplot2::theme(
    title = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    plot.title.position = "plot",
    panel.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(colour = "#E0E1E6"),
    axis.line.y = element_line(colour = "#E0E1E6")
  )






