library(conflicted)
library(tidyverse)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

traindata = "train_data.csv" |>
	read_csv() |>
	mutate(
		across(
			.cols = where(is.character),
			.fns = as.factor
		)
	) |>
	mutate(
		died_at_hospital = ExpiredHospital |>
			factor(levels = c("0", "1")),
		LOSgroupNum = NULL, # LOSgroupNum is determined completely by LOSdays
		.keep = "unused"
	)

summary(traindata)
str(traindata)

maxdays = 16

maxedout = traindata |>
	filter(LOSdays <= maxdays)

ggplot(
	data = maxedout
) + theme_classic() + geom_violin(
	mapping = aes(
		y = died_at_hospital,
		x = LOSdays,
		fill = died_at_hospital,
		colour = died_at_hospital
	),
	draw_quantiles = c(0.25, 0.5, 0.75)
) + scale_fill_viridis_d(
	alpha = 0.5,
	option = "turbo",
	end = 0.7,
	guide = "none"
) + scale_colour_viridis_d(
	option = "turbo",
	end = 0.7,
	guide = "none"
) + labs(
	title = "Patients' length of hospital stay",
	subtitle = "up to 16 days, by patient status",
	caption = "violins are scaled for equal area"
) + scale_x_continuous(
	name = "days in hospital",
	breaks = c(0, 4, 8, 12, 16),
	minor_breaks = NULL
) + scale_y_discrete(
	name = "",
	labels = c(
		"0" = "discharged",
		"1" = "died"
	)
) + theme(
	plot.title = element_text(size = rel(2)),
	axis.line = element_blank(),
	axis.ticks.y = element_blank()
)

ggplot(
	data = maxedout
) + geom_violin(
	mapping = aes(
		y = insurance,
		x = LOSdays,
		fill = insurance,
		colour = insurance
	),
	draw_quantiles = c(0.25, 0.5, 0.75)
) + scale_fill_viridis_d(
	alpha = 0.5,
	option = "plasma",
	guide = "none"
) + scale_colour_viridis_d(
	option = "plasma",
	guide = "none"
) + labs(
	title = "Patients' length of hospital stay",
	subtitle = "up to 16 days, by insurance type",
	caption = "violins are scaled for equal area"
) + scale_x_continuous(
	name = "days in hospital",
	breaks = c(0, 4, 8, 12, 16),
	minor_breaks = NULL
) + scale_y_discrete(
	name = "",
	labels = c(
		"Self Pay" = "self pay",
		"Private" = "private",
		"Medicare" = "medicare",
		"Medicaid" = "medicaid",
		"Government" = "government"
	)
) + theme_classic() + theme(
	plot.title = element_text(size = rel(2)),
	axis.line = element_blank(),
	axis.ticks.y = element_blank()
)

ggplot(
	data = maxedout
) + geom_violin(
	mapping = aes(
		y = admit_type,
		x = LOSdays,
		fill = admit_type,
		colour = admit_type
	),
	draw_quantiles = c(0.25, 0.5, 0.75),
	width = 1.35
) + scale_fill_viridis_d(
	alpha = 0.5,
	option = "viridis",
	guide = "none"
) + scale_colour_viridis_d(
	option = "viridis",
	guide = "none"
) + labs(
	title = "Patients' length of hospital stay",
	subtitle = "up to 16 days, by hospital admission type",
	caption = "violins are scaled for equal area"
) + scale_x_continuous(
	name = "days in hospital",
	breaks = c(0, 4, 8, 12, 16),
	minor_breaks = NULL
) + scale_y_discrete(
	name = "",
	labels = c(
		"URGENT" = "urgent care",
		"NEWBORN" = "newborn",
		"EMERGENCY" = "emergency",
		"ELECTIVE" = "elective"
	)
) + theme_classic() + theme(
	plot.title = element_text(size = rel(2)),
	axis.line = element_blank(),
	axis.ticks.y = element_blank()
)