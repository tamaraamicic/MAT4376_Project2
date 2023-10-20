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
		LOSgroupNum = NULL, # remove this column; LOSgroupNum is determined completely by LOSdays
		.keep = "unused"
	)

summary(traindata)
str(traindata)

maxdays = 15

maxedout = traindata |>
	filter(LOSdays <= maxdays)

ggplot(
	data = maxedout
) + geom_violin(
	mapping = aes(
		x = died_at_hospital,
		y = LOSdays
	)
)

ggplot(
	data = maxedout
) + geom_violin(
	mapping = aes(
		x = insurance,
		y = LOSdays
	)
)

ggplot(
	data = maxedout
) + geom_violin(
	mapping = aes(
		x = admit_type,
		y = LOSdays
	)
)