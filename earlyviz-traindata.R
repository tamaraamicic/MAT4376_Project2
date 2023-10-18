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
		ExpiredHospital = ExpiredHospital |>
			factor(levels = c("0", "1")),
		LOSgroupNum = LOSgroupNum |>
			as.factor()
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
		x = ExpiredHospital,
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

ggplot(
	data = traindata
) + geom_freqpoly(
	mapping = aes()
)