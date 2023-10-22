library(conflicted)
library(tidytable)
library(tidyverse)
"dplyr" |>
	conflict_prefer_all(quiet = TRUE)

traindata_ids = "train_data.csv" |>
	read_csv() |>
	pull(hadm_id)
mimic = "mimic3d.csv" |>
	read_csv() |>
	mutate(
		across(
			.cols = where(is.character),
			.fns = as.factor
		)
	) |>
	get_dummies(
		cols = AdmitProcedure,
		prefix = TRUE,
		prefix_sep = "_"
	) |>
	select(!LOSgroupNum) |> # remove: determined completely by LOSdays
	mutate(
		sex_M = gender |>
			str_detect("M") |>
			if_else(1, 0),
		sex_F = gender |>
			str_detect("F") |>
			if_else(1, 0),
		.keep = "unused", # replace gender as a column
		.after = "hadm_id"
	) |>
	mutate(
		admit_location_not_available = admit_location |>
			str_detect("^.. INFO NOT AVAILABLE ..$") |>
			if_else(1, 0),
		admit_location_premature = admit_location |>
			str_detect("^CLINIC REFERRAL/PREMATURE$") |>
			if_else(1, 0),
		admit_location_emergency = admit_location |>
			str_detect("^EMERGENCY ROOM ADMIT$") |>
			if_else(1, 0),
		admit_location_HMO_sick = admit_location |>
			str_detect("^HMO REFERRAL/SICK$") |>
			if_else(1, 0),
		admit_location_physician_referral_normal_delivery = admit_location |>
			str_detect("^PHYS REFERRAL/NORMAL DELI$") |>
			if_else(1, 0),
		admit_location_hospital_extram = admit_location |>
			str_detect("^TRANSFER FROM HOSP/EXTRAM$") |>
			if_else(1, 0),
		admit_location_another_facility = admit_location |>
			str_detect("^TRANSFER FROM ANOTHER HEALT$") |>
			if_else(1, 0),
		admit_location_skilled_nurse = admit_location |>
			str_detect("^TRANSFER FROM SKILLED NUR$") |>
			if_else(1, 0),
		admit_location_same_facility = admit_location |>
			str_detect("^TRSF WITHIN THIS FACILITY$") |>
			if_else(1, 0),
		.keep = "unused",
		.after = "LOSdays"
	) |>
	mutate(
		admit_type_elective = admit_type |>
			str_detect("^ELECTIVE$") |>
			if_else(1, 0),
		admit_type_emergency = admit_type |>
			str_detect("^EMERGENCY$") |>
			if_else(1, 0),
		admit_type_newborn = admit_type |>
			str_detect("^NEWBORN$") |>
			if_else(1, 0),
		admit_type_urgent = admit_type |>
			str_detect("^URGENT$") |>
			if_else(1, 0),
		.keep = "unused"
	) |>
	mutate(
		insurance_government = insurance |>
			str_detect("^Government$") |>
			if_else(1, 0),
		insurance_medicaid = insurance |>
			str_detect("^Medicaid$") |>
			if_else(1, 0),
		insurance_medicare = insurance |>
			str_detect("^Medicare$") |>
			if_else(1, 0),
		insurance_private = insurance |>
			str_detect("^Private$") |>
			if_else(1, 0),
		insurance_self_pay = insurance |>
			str_detect("^Self Pay$") |>
			if_else(1, 0),
		.keep = "unused"
	) |>
	mutate(
		religion_seventh_day_adventist = religion |>
			str_detect("^7TH DAY ADVENTIST$") |>
			if_else(1, 0),
		religion_baptist = religion |>
			str_detect("^BAPTIST$") |>
			if_else(1, 0),
		religion_buddhist = religion |>
			str_detect("^BUDDHST$") |>
			if_else(1, 0),
		religion_catholic = religion |>
			str_detect("^CATHOLIC$") |>
			if_else(1, 0),
		religion_christian_scientist = religion |>
			str_detect("^CHRISTIAN SCIENTIST$") |>
			if_else(1, 0),
		religion_episcopalian = religion |>
			str_detect("^EPISCOPALIAN$") |>
			if_else(1, 0),
		religion_greek_orthodox = religion |>
			str_detect("^GREEK ORTHODOX$") |>
			if_else(1, 0),
		religion_hebrew = religion |>
			str_detect("^HEBREW$") |>
			if_else(1, 0),
		religion_hindu = religion |>
			str_detect("^HINDU$") |>
			if_else(1, 0),
		religion_witness = religion |>
			str_detect("^JEHOVAH'S WITNESS$") |>
			if_else(1, 0),
		religion_jewish = religion |>
			str_detect("^JEWISH$") |>
			if_else(1, 0),
		religion_methodist = religion |>
			str_detect("^METHODIST$") |>
			if_else(1, 0),
		religion_muslim = religion |>
			str_detect("^MUSLIM$") |>
			if_else(1, 0),
		religion_not_specified = religion |>
			str_detect("^NOT SPECIFIED$") |>
			if_else(1, 0),
		religion_other = religion |>
			str_detect("^OTHER$") |>
			if_else(1, 0),
		religion_protestant_quaker = religion |>
			str_detect("^PROTESTANT QUAKER$") |>
			if_else(1, 0),
		religion_romanian_east_orthodox = religion |>
			str_detect("^ROMANIAN EAST. ORTH$") |>
			if_else(1, 0),
		religion_unitarian_universalist = religion |>
			str_detect("^UNITARIAN-UNIVERSALIST$") |>
			if_else(1, 0),
		religion_unobtainable = religion |>
			str_detect("^UNOBTAINABLE$") |>
			if_else(1, 0),
		.keep = "unused",
		.after = "insurance_self_pay"
	) |>
	mutate(
		marital_status_divorced = marital_status |>
			str_detect("^DIVORCED$") |>
			if_else(1, 0),
		marital_status_life_partner = marital_status |>
			str_detect("^LIFE PARTNER$") |>
			if_else(1, 0),
		marital_status_married = marital_status |>
			str_detect("^MARRIED$") |>
			if_else(1, 0),
		marital_status_seperated = marital_status |>
			str_detect("^SEPERATED$") |>
			if_else(1, 0),
		marital_status_single = marital_status |>
			str_detect("^SINGLE$") |>
			if_else(1, 0),
		marital_status_unknown = marital_status |>
			str_detect("^UNKNOWN (DEFAULT)$") |>
			if_else(1, 0),
		marital_status_widowed = marital_status |>
			str_detect("^WIDOWED$") |>
			if_else(1, 0),
		.keep = "unused",
		.after = "religion_unobtainable"
	) |>
	mutate(
		eth_amerindian = ethnicity |>
			str_detect("^AMERICAN INDIAN/ALASKA NATIVE$") |>
			if_else(1, 0),
		eth_recog_amerindian = ethnicity |>
			str_detect("^AMERICAN INDIAN/ALASKA NATIVE FEDERALLY RECOGNIZED TRIBE$") |>
			if_else(1, 0),
		eth_asian = ethnicity |>
			str_detect("^ASIAN$") |>
			if_else(1, 0),
		eth_asian_indian = ethnicity |>
			str_detect("^ASIAN - ASIAN INDIAN$") |>
			if_else(1, 0),
		eth_asian_cambodian = ethnicity |>
			str_detect("^ASIAN - CAMBODIAN$") |>
			if_else(1, 0),
		eth_asian_chinese = ethnicity |>
			str_detect("^ASIAN - CHINESE$") |>
			if_else(1, 0),
		eth_asian_filipino = ethnicity |>
			str_detect("^ASIAN - FILIPINO$") |>
			if_else(1, 0),
		eth_asian_japanese = ethnicity |>
			str_detect("^ASIAN - JAPANESE$") |>
			if_else(1, 0),
		eth_asian_korean = ethnicity |>
			str_detect("^ASIAN - KOREAN$") |>
			if_else(1, 0),
		eth_asian_other = ethnicity |>
			str_detect("^ASIAN - OTHER$") |>
			if_else(1, 0),
		eth_asian_thai = ethnicity |>
			str_detect("^ASIAN - THAI$") |>
			if_else(1, 0),
		eth_asian_vietnamese = ethnicity |>
			str_detect("^ASIAN - VIETNAMESE$") |>
			if_else(1, 0),
		eth_black_african = ethnicity |>
			str_detect("^BLACK/AFRICAN$") |>
			if_else(1, 0),
		eth_black_african_american = ethnicity |>
			str_detect("^BLACK/AFRICAN AMERICAN$") |>
			if_else(1, 0),
		eth_black_cape_verdean = ethnicity |>
			str_detect("^BLACK/CAPE VERDEAN$") |>
			if_else(1, 0),
		eth_black_haitian = ethnicity |>
			str_detect("^BLACK/HAITIAN$") |>
			if_else(1, 0),
		eth_caribbean_island = ethnicity |>
			str_detect("^CARIBBEAN ISLAND$") |>
			if_else(1, 0),
		eth_hispanic_latino = ethnicity |>
			str_detect("^HISPANIC OR LATINO$") |>
			if_else(1, 0),
		eth_hispanic_centroamerican_other = ethnicity |>
			str_detect("^HISPANIC/LATINO - CENTRAL AMERICAN (OTHER)$") |>
			if_else(1, 0),
		eth_hispanic_colombian = ethnicity |>
			str_detect("^HISPANIC/LATINO - COLOMBIAN$") |>
			if_else(1, 0),
		eth_hispanic_cuban = ethnicity |>
			str_detect("^HISPANIC/LATINO - CUBAN$") |>
			if_else(1, 0),
		eth_hispanic_dominican = ethnicity |>
			str_detect("^HISPANIC/LATINO - DOMINICAN$") |>
			if_else(1, 0),
		eth_hispanic_guatemalan = ethnicity |>
			str_detect("^HISPANIC/LATINO - GUATEMALAN$") |>
			if_else(1, 0),
		eth_hispanic_honduran = ethnicity |>
			str_detect("^HISPANIC/LATINO - HONDURAN$") |>
			if_else(1, 0),
		eth_hispanic_mexican = ethnicity |>
			str_detect("^HISPANIC/LATINO - MEXICAN$") |>
			if_else(1, 0),
		eth_hispanic_puerto_rican = ethnicity |>
			str_detect("^HISPANIC/LATINO - PUERTO RICAN$") |>
			if_else(1, 0),
		eth_hispanic_salvadoran = ethnicity |>
			str_detect("^HISPANIC/LATINO - SALVADORAN$") |>
			if_else(1, 0),
		eth_middle_eastern = ethnicity |>
			str_detect("^MIDDLE EASTERN$") |>
			if_else(1, 0),
		eth_multi_race = ethnicity |>
			str_detect("^MULTI RACE ETHNICITY$") |>
			if_else(1, 0),
		eth_pacific_islander = ethnicity |>
			str_detect("^NATIVE HAWAIIAN OR PACIFIC ISLANDER$") |>
			if_else(1, 0),
		eth_other = ethnicity |>
			str_detect("^OTHER$") |>
			if_else(1, 0),
		eth_declined_answer = ethnicity |>
			str_detect("^PATIENT DECLINED TO ANSWER$") |>
			if_else(1, 0),
		eth_portuguese = ethnicity |>
			str_detect("^PORTUGUESE$") |>
			if_else(1, 0),
		eth_south_american = ethnicity |>
			str_detect("^SOUTH AMERICAN$") |>
			if_else(1, 0),
		eth_unable_to_obtain = ethnicity |>
			str_detect("^UNABLE TO OBTAIN$") |>
			if_else(1, 0),
		eth_unknown_not_specified = ethnicity |>
			str_detect("^UNKNOWN/NOT SPECIFIED") |>
			if_else(1, 0),
		eth_white = ethnicity |>
			str_detect("^WHITE$") |>
			if_else(1, 0),
		eth_white_brazilian = ethnicity |>
			str_detect("^WHITE - BRAZILIAN$") |>
			if_else(1, 0),
		eth_white_eastern_european = ethnicity |>
			str_detect("^WHITE - EASTERN EUROPEAN$") |>
			if_else(1, 0),
		eth_white_other_european = ethnicity |>
			str_detect("^WHITE - OTHER EUROPEAN$") |>
			if_else(1, 0),
		eth_white_russian = ethnicity |>
			str_detect("^WHITE - RUSSIAN$") |>
			if_else(1, 0),
		.keep = "unused",
		.after = "marital_status_widowed"
	)
traindata = mimic |>
	filter(hadm_id %in% traindata_ids)
testdata = mimic |>
	filter(hadm_id %notin% traindata_ids)
traindata |>
	fwrite(file = "./transformed_train_data.csv")
testdata |>
	fwrite(file = "./transformed_test_data.csv")