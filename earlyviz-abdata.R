library(conflicted)
library(tidyverse)
	conflict_prefer_all("dplyr", quiet = TRUE)

abdata = "ab_data.csv" |>
	read_csv(
		col_types = list(
			user_id = col_integer(),
			timestamp = col_datetime(),
			converted = col_factor()
		)
	)

abdata |>
	count(group, landing_page, converted)
summary(abdata)

abdata |>
	filter(landing_page == "old_page") |>
	summary()

abdata |>
	filter(group == "control") |>
	summary()

totals = abdata |>
	count(group, landing_page) |>
	mutate(total = n, .keep = "unused")
proportions = abdata |>
	count(group, landing_page, converted) |>
	inner_join(totals) |>
	mutate(prop = n/total, .keep = "unused")

ggplot(
	data = proportions |>
		filter(converted == 1)
) + geom_point(
	mapping = aes(
		x = landing_page,
		y = group,
		colour = prop,
		size = prop
	)
)

regready = abdata |>
	mutate(
		landing_page = landing_page |>
			case_match("old_page" ~ 0, "new_page" ~ 1) |>
			as.numeric(),
		group = group |>
			case_match("control" ~ 0, "treatment" ~ 1) |>
			as.numeric(),
		timestamp = as.double(timestamp) - min(as.double(timestamp))
	)
reg = lm(converted ~ group*landing_page*timestamp, data = regready)
summary(reg)

