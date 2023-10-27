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
	count(group, landing_page)

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
# calculate the proportions of each population that converted
proportions = abdata |>
	count(group, landing_page, converted) |>
	inner_join(totals) |>
	mutate(prop = n/total)

ggplot(
	data = proportions |>
		filter(converted == 1)
) + geom_count(
	mapping = aes(
		x = landing_page,
		y = group,
		colour = prop,
		size = total
	)
) + scale_size_area(
	max_size = 40,
	guide = NULL
) + scale_colour_viridis_c(
	begin = 0.25
) + theme_classic() + labs(
	title = "Store membership signup rates",
	subtitle = "by landing page and website used",
	colour = "signup rate"
) + scale_x_discrete(
	name = "",
	limits = c("old_page", "new_page"),
	labels = c(
		"old_page" = "used old site",
		"new_page" = "used new site"
	)
) + scale_y_discrete(
	name = "",
	limits = c("treatment", "control"),
	labels = c(
		"control" = "old\nlanding\npage",
		"treatment" = "new\nlanding\npage"
	)
) + theme(
	plot.title = element_text(size = rel(2)),
	axis.line = element_blank(),
	axis.ticks = element_blank()
) + geom_text(
	mapping = aes(
		x = landing_page,
		y = group,
		label = total
	),
	nudge_y = 0.175
)
