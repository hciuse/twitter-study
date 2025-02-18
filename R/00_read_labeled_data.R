data <- read.csv(here("raw_data", "labeled_raw_data.csv"), quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "seed"
names(data)[5] <- "seed"
# LimeSurvey Field type: DATETIME23.2
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "startdate"
names(data)[6] <- "startdate"
# LimeSurvey Field type: DATETIME23.2
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "datestamp"
names(data)[7] <- "datestamp"
# LimeSurvey Field type: A
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "ipaddr"
names(data)[8] <- "ipaddr"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- "refurl"
names(data)[9] <- "refurl"
# LimeSurvey Field type: F
data[, 10] <- as.numeric(data[, 10])
attributes(data)$variable.labels[10] <- "Sind Sie volljährig?"
data[, 10] <- factor(data[, 10], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[10] <- "AC"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- ""
names(data)[11] <- "ID"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- ""
names(data)[12] <- "ref"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- ""
names(data)[13] <- "origin"
# LimeSurvey Field type: A
data[, 14] <- as.character(data[, 14])
attributes(data)$variable.labels[14] <- "Bitte geben Sie Ihr Geschlecht an."
data[, 14] <- factor(data[, 14], levels=c("A1","A2","A3","A4"),labels=c("Männlich", "Weiblich", "Divers", "Ich möchte nicht antworten"))
names(data)[14] <- "A4"
# LimeSurvey Field type: F
data[, 15] <- as.numeric(data[, 15])
attributes(data)$variable.labels[15] <- "In welchem Jahr sind Sie geboren?"
names(data)[15] <- "A5"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "Wie häufig waren Sie nachgewiesenermaßen (positives Testergebnis) mit SARS-CoV-2/ COVID-19 infiziert?"
data[, 16] <- factor(data[, 16], levels=c("A1","A2","A3","A4","A5","A6"),labels=c("Nie", "Einmal", "Zweimal", "Dreimal", "Mehr als dreimal", "Ich möchte nicht antworten"))
names(data)[16] <- "B1"
# LimeSurvey Field type: F
data[, 17] <- as.numeric(data[, 17])
attributes(data)$variable.labels[17] <- "[Selbsttest (zu Hause)] Wie wurde die erste Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 17] <- factor(data[, 17], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[17] <- "B1S2E_SQ001"
# LimeSurvey Field type: F
data[, 18] <- as.numeric(data[, 18])
attributes(data)$variable.labels[18] <- "[Schnelltest im Testzentrum] Wie wurde die erste Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 18] <- factor(data[, 18], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[18] <- "B1S2E_SQ002"
# LimeSurvey Field type: F
data[, 19] <- as.numeric(data[, 19])
attributes(data)$variable.labels[19] <- "[PCR Test beim Arzt/Ärztin] Wie wurde die erste Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 19] <- factor(data[, 19], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[19] <- "B1S2E_SQ003"
# LimeSurvey Field type: F
data[, 20] <- as.numeric(data[, 20])
attributes(data)$variable.labels[20] <- "[PCR Test beim Testzentrum] Wie wurde die erste Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 20] <- factor(data[, 20], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[20] <- "B1S2E_SQ004"
# LimeSurvey Field type: DATETIME23.2
data[, 21] <- as.character(data[, 21])
attributes(data)$variable.labels[21] <- "Wann wurde die erste Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?Falls Sie das genaue Datum nicht mehr kennen, geben Sie ein ungefähres Datum an."
names(data)[21] <- "B1S1E"
# LimeSurvey Field type: F
data[, 22] <- as.numeric(data[, 22])
attributes(data)$variable.labels[22] <- "[Selbsttest (zu Hause)] Wie wurde die zweite Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 22] <- factor(data[, 22], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[22] <- "B1S2Z_SQ001"
# LimeSurvey Field type: F
data[, 23] <- as.numeric(data[, 23])
attributes(data)$variable.labels[23] <- "[Schnelltest im Testzentrum] Wie wurde die zweite Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 23] <- factor(data[, 23], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[23] <- "B1S2Z_SQ002"
# LimeSurvey Field type: F
data[, 24] <- as.numeric(data[, 24])
attributes(data)$variable.labels[24] <- "[PCR Test beim Arzt/Ärztin] Wie wurde die zweite Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 24] <- factor(data[, 24], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[24] <- "B1S2Z_SQ003"
# LimeSurvey Field type: F
data[, 25] <- as.numeric(data[, 25])
attributes(data)$variable.labels[25] <- "[PCR Test beim Testzentrum] Wie wurde die zweite Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 25] <- factor(data[, 25], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[25] <- "B1S2Z_SQ004"
# LimeSurvey Field type: DATETIME23.2
data[, 26] <- as.character(data[, 26])
attributes(data)$variable.labels[26] <- "Wann wurde die zweite Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt? Falls Sie das genaue Datum nicht mehr kennen, geben Sie ein ungefähres Datum an."
names(data)[26] <- "B1S1Z"
# LimeSurvey Field type: F
data[, 27] <- as.numeric(data[, 27])
attributes(data)$variable.labels[27] <- "[Selbsttest (zu Hause)] Wie wurde die dritte Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 27] <- factor(data[, 27], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[27] <- "B1S2D_SQ001"
# LimeSurvey Field type: F
data[, 28] <- as.numeric(data[, 28])
attributes(data)$variable.labels[28] <- "[Schnelltest im Testzentrum] Wie wurde die dritte Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 28] <- factor(data[, 28], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[28] <- "B1S2D_SQ002"
# LimeSurvey Field type: F
data[, 29] <- as.numeric(data[, 29])
attributes(data)$variable.labels[29] <- "[PCR Test beim Arzt/Ärztin] Wie wurde die dritte Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 29] <- factor(data[, 29], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[29] <- "B1S2D_SQ003"
# LimeSurvey Field type: F
data[, 30] <- as.numeric(data[, 30])
attributes(data)$variable.labels[30] <- "[PCR Test beim Testzentrum] Wie wurde die dritte Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?"
data[, 30] <- factor(data[, 30], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[30] <- "B1S2D_SQ004"
# LimeSurvey Field type: DATETIME23.2
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "Wann wurde die dritte Infektion mit dem Coronavirus (SARS-CoV-2/ COVID 19) festgestellt?Falls Sie das genaue Datum nicht mehr kennen, geben Sie ein ungefähres Datum an."
names(data)[31] <- "B1S1D"
# LimeSurvey Field type: F
data[, 32] <- as.numeric(data[, 32])
attributes(data)$variable.labels[32] <- "[Im eigenen Haushalt] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 32] <- factor(data[, 32], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[32] <- "BN1_SQ001"
# LimeSurvey Field type: F
data[, 33] <- as.numeric(data[, 33])
attributes(data)$variable.labels[33] <- "[Auf der Arbeit] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 33] <- factor(data[, 33], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[33] <- "BN1_SQ002"
# LimeSurvey Field type: F
data[, 34] <- as.numeric(data[, 34])
attributes(data)$variable.labels[34] <- "[In der Schule] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 34] <- factor(data[, 34], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[34] <- "BN1_SQ003"
# LimeSurvey Field type: F
data[, 35] <- as.numeric(data[, 35])
attributes(data)$variable.labels[35] <- "[Treffen mit Freunden oder Bekannten] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 35] <- factor(data[, 35], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[35] <- "BN1_SQ004"
# LimeSurvey Field type: F
data[, 36] <- as.numeric(data[, 36])
attributes(data)$variable.labels[36] <- "[Sport / Freizeitveranstaltung] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 36] <- factor(data[, 36], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[36] <- "BN1_SQ005"
# LimeSurvey Field type: F
data[, 37] <- as.numeric(data[, 37])
attributes(data)$variable.labels[37] <- "[Arztpraxis / Wartezimmer / Krankenhaus] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 37] <- factor(data[, 37], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[37] <- "BN1_SQ006"
# LimeSurvey Field type: F
data[, 38] <- as.numeric(data[, 38])
attributes(data)$variable.labels[38] <- "[Einkaufen] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 38] <- factor(data[, 38], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[38] <- "BN1_SQ007"
# LimeSurvey Field type: F
data[, 39] <- as.numeric(data[, 39])
attributes(data)$variable.labels[39] <- "[Große Feier / Party] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 39] <- factor(data[, 39], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[39] <- "BN1_SQ008"
# LimeSurvey Field type: F
data[, 40] <- as.numeric(data[, 40])
attributes(data)$variable.labels[40] <- "[Konferenz / Workshop] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 40] <- factor(data[, 40], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[40] <- "BN1_SQ009"
# LimeSurvey Field type: F
data[, 41] <- as.numeric(data[, 41])
attributes(data)$variable.labels[41] <- "[Unbekannt] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 41] <- factor(data[, 41], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[41] <- "BN1_SQ010"
# LimeSurvey Field type: F
data[, 42] <- as.numeric(data[, 42])
attributes(data)$variable.labels[42] <- "[keine Angabe] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
data[, 42] <- factor(data[, 42], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[42] <- "BN1_SQ011"
# LimeSurvey Field type: A
data[, 43] <- as.character(data[, 43])
attributes(data)$variable.labels[43] <- "[Sonstiges] Wo haben Sie sich bei allen Ihren Infektionen angesteckt?"
names(data)[43] <- "BN1_other"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "Waren Sie auf Grund einer Coronavirus (SARS-CoV-2/ COVID 19) Erkrankung im Krankenhaus?(Falls es mehrere Krankenhausaufenthalte waren, beantworten Sie diese Frage bitte für den ersten Aufenthalt.)"
data[, 44] <- factor(data[, 44], levels=c("A1","A2","A3"),labels=c("Ja", "Nein", "Ich möchte nicht antworten"))
names(data)[44] <- "B18"
# LimeSurvey Field type: F
data[, 45] <- as.numeric(data[, 45])
attributes(data)$variable.labels[45] <- "Wenn ja, wie lange waren Sie im Krankenhaus? (Anzahl der Tage)"
names(data)[45] <- "B18S1"
# LimeSurvey Field type: F
data[, 46] <- as.numeric(data[, 46])
attributes(data)$variable.labels[46] <- "[Bluthochdruck (Hypertonie)] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 46] <- factor(data[, 46], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[46] <- "B21_SQ001"
# LimeSurvey Field type: F
data[, 47] <- as.numeric(data[, 47])
attributes(data)$variable.labels[47] <- "[Zuckerkrankheit (Diabetes)] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 47] <- factor(data[, 47], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[47] <- "B21_SQ002"
# LimeSurvey Field type: F
data[, 48] <- as.numeric(data[, 48])
attributes(data)$variable.labels[48] <- "[Herz-Kreislauf-Erkrankung (z.B. koronare Herzkrankheit, Zustand nach Herzinfarkt, Herzinsuffizienz/Herzschwäche, Herzrhythmusstörung, Zustand nach Schlaganfall) ] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 48] <- factor(data[, 48], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[48] <- "B21_SQ003"
# LimeSurvey Field type: F
data[, 49] <- as.numeric(data[, 49])
attributes(data)$variable.labels[49] <- "[chronische Lungenerkrankung (z.B. Asthma, chronische Bronchitis, chronisch-obstruktive Lungenerkrankung (COPD), Lungenemphysem) ] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 49] <- factor(data[, 49], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[49] <- "B21_SQ004"
# LimeSurvey Field type: F
data[, 50] <- as.numeric(data[, 50])
attributes(data)$variable.labels[50] <- "[gegenwärtig Immunschwäche (z.B. aufgrund einer Erkrankung, Organtransplantation, Chemotherapie oder derzeitigen Einnahme anderer Medikamente wie Cortison) ] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 50] <- factor(data[, 50], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[50] <- "B21_SQ005"
# LimeSurvey Field type: F
data[, 51] <- as.numeric(data[, 51])
attributes(data)$variable.labels[51] <- "[Krebserkrankung, wegen der Sie gegenwärtig oder im letzten Jahr behandelt wurden ] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 51] <- factor(data[, 51], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[51] <- "B21_SQ006"
# LimeSurvey Field type: F
data[, 52] <- as.numeric(data[, 52])
attributes(data)$variable.labels[52] <- "[Post-COVID-19 Zustand] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 52] <- factor(data[, 52], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[52] <- "B21_SQ007"
# LimeSurvey Field type: F
data[, 53] <- as.numeric(data[, 53])
attributes(data)$variable.labels[53] <- "[Nein, keine dieser Erkrankungen  ] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 53] <- factor(data[, 53], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[53] <- "B21_SQ009"
# LimeSurvey Field type: F
data[, 54] <- as.numeric(data[, 54])
attributes(data)$variable.labels[54] <- "[Ich möchte nicht antworten] Haben Sie eine der folgenden, durch einen Arzt oder eine Ärztin festgestellten, Erkrankungen?"
data[, 54] <- factor(data[, 54], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[54] <- "B21_SQ008"
# LimeSurvey Field type: F
data[, 55] <- as.numeric(data[, 55])
attributes(data)$variable.labels[55] <- "[Im Jahr 2019] [Anzahl Personen, die im Haushalt gelebt haben.] Wieviele Personen haben in den folgenden Zeiträumen in Ihrem Haushalt gelebt?   "
names(data)[55] <- "Z1Q11_SQ001_SQ001"
# LimeSurvey Field type: F
data[, 56] <- as.numeric(data[, 56])
attributes(data)$variable.labels[56] <- "[Ende März 2020] [Anzahl Personen, die im Haushalt gelebt haben.] Wieviele Personen haben in den folgenden Zeiträumen in Ihrem Haushalt gelebt?   "
names(data)[56] <- "Z1Q11_SQ002_SQ001"
# LimeSurvey Field type: F
data[, 57] <- as.numeric(data[, 57])
attributes(data)$variable.labels[57] <- "[Im Sommer 2021] [Anzahl Personen, die im Haushalt gelebt haben.] Wieviele Personen haben in den folgenden Zeiträumen in Ihrem Haushalt gelebt?   "
names(data)[57] <- "Z1Q11_SQ003_SQ001"
# LimeSurvey Field type: F
data[, 58] <- as.numeric(data[, 58])
attributes(data)$variable.labels[58] <- "[Januar 2023] [Anzahl Personen, die im Haushalt gelebt haben.] Wieviele Personen haben in den folgenden Zeiträumen in Ihrem Haushalt gelebt?   "
names(data)[58] <- "Z1Q11_SQ004_SQ001"
# LimeSurvey Field type: F
data[, 59] <- as.numeric(data[, 59])
attributes(data)$variable.labels[59] <- "[Im Jahr 2019] [Ihre Kontakte im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[59] <- "Z1Q1_SQ001_SQ002"
# LimeSurvey Field type: F
data[, 60] <- as.numeric(data[, 60])
attributes(data)$variable.labels[60] <- "[Im Jahr 2019] [Ihre Kontakte im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[60] <- "Z1Q1_SQ001_SQ003"
# LimeSurvey Field type: F
data[, 61] <- as.numeric(data[, 61])
attributes(data)$variable.labels[61] <- "[Im Jahr 2019] [Ihre Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[61] <- "Z1Q1_SQ001_SQ004"
# LimeSurvey Field type: F
data[, 62] <- as.numeric(data[, 62])
attributes(data)$variable.labels[62] <- "[Ende März 2020] [Ihre Kontakte im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[62] <- "Z1Q1_SQ002_SQ002"
# LimeSurvey Field type: F
data[, 63] <- as.numeric(data[, 63])
attributes(data)$variable.labels[63] <- "[Ende März 2020] [Ihre Kontakte im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[63] <- "Z1Q1_SQ002_SQ003"
# LimeSurvey Field type: F
data[, 64] <- as.numeric(data[, 64])
attributes(data)$variable.labels[64] <- "[Ende März 2020] [Ihre Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[64] <- "Z1Q1_SQ002_SQ004"
# LimeSurvey Field type: F
data[, 65] <- as.numeric(data[, 65])
attributes(data)$variable.labels[65] <- "[Im Sommer 2021] [Ihre Kontakte im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[65] <- "Z1Q1_SQ003_SQ002"
# LimeSurvey Field type: F
data[, 66] <- as.numeric(data[, 66])
attributes(data)$variable.labels[66] <- "[Im Sommer 2021] [Ihre Kontakte im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[66] <- "Z1Q1_SQ003_SQ003"
# LimeSurvey Field type: F
data[, 67] <- as.numeric(data[, 67])
attributes(data)$variable.labels[67] <- "[Im Sommer 2021] [Ihre Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[67] <- "Z1Q1_SQ003_SQ004"
# LimeSurvey Field type: F
data[, 68] <- as.numeric(data[, 68])
attributes(data)$variable.labels[68] <- "[Januar 2023] [Ihre Kontakte im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[68] <- "Z1Q1_SQ004_SQ002"
# LimeSurvey Field type: F
data[, 69] <- as.numeric(data[, 69])
attributes(data)$variable.labels[69] <- "[Januar 2023] [Ihre Kontakte im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[69] <- "Z1Q1_SQ004_SQ003"
# LimeSurvey Field type: F
data[, 70] <- as.numeric(data[, 70])
attributes(data)$variable.labels[70] <- "[Januar 2023] [Ihre Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatten Sie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls Sie zu einem dieser Zeitpunkte noch Schüler*in waren, tragen Sie bitte Ihre Kontakte in der Spalte \"Ihre Kontakte im Zusammenhang mit Schule/Kita\" ein.   "
names(data)[70] <- "Z1Q1_SQ004_SQ004"
# LimeSurvey Field type: F
data[, 71] <- as.numeric(data[, 71])
attributes(data)$variable.labels[71] <- "[Im Jahr 2019] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Arbeit/Uni] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[71] <- "Z1Q2_SQ001_SQ002"
# LimeSurvey Field type: F
data[, 72] <- as.numeric(data[, 72])
attributes(data)$variable.labels[72] <- "[Im Jahr 2019] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Schule/Kita] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[72] <- "Z1Q2_SQ001_SQ003"
# LimeSurvey Field type: F
data[, 73] <- as.numeric(data[, 73])
attributes(data)$variable.labels[73] <- "[Im Jahr 2019] [Kontakte der Haushaltsmitglieder in der Freizeit] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[73] <- "Z1Q2_SQ001_SQ004"
# LimeSurvey Field type: F
data[, 74] <- as.numeric(data[, 74])
attributes(data)$variable.labels[74] <- "[Ende März 2020] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Arbeit/Uni] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[74] <- "Z1Q2_SQ002_SQ002"
# LimeSurvey Field type: F
data[, 75] <- as.numeric(data[, 75])
attributes(data)$variable.labels[75] <- "[Ende März 2020] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Schule/Kita] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[75] <- "Z1Q2_SQ002_SQ003"
# LimeSurvey Field type: F
data[, 76] <- as.numeric(data[, 76])
attributes(data)$variable.labels[76] <- "[Ende März 2020] [Kontakte der Haushaltsmitglieder in der Freizeit] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[76] <- "Z1Q2_SQ002_SQ004"
# LimeSurvey Field type: F
data[, 77] <- as.numeric(data[, 77])
attributes(data)$variable.labels[77] <- "[Im Sommer 2021] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Arbeit/Uni] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[77] <- "Z1Q2_SQ003_SQ002"
# LimeSurvey Field type: F
data[, 78] <- as.numeric(data[, 78])
attributes(data)$variable.labels[78] <- "[Im Sommer 2021] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Schule/Kita] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[78] <- "Z1Q2_SQ003_SQ003"
# LimeSurvey Field type: F
data[, 79] <- as.numeric(data[, 79])
attributes(data)$variable.labels[79] <- "[Im Sommer 2021] [Kontakte der Haushaltsmitglieder in der Freizeit] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[79] <- "Z1Q2_SQ003_SQ004"
# LimeSurvey Field type: F
data[, 80] <- as.numeric(data[, 80])
attributes(data)$variable.labels[80] <- "[Januar 2023] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Arbeit/Uni] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[80] <- "Z1Q2_SQ004_SQ002"
# LimeSurvey Field type: F
data[, 81] <- as.numeric(data[, 81])
attributes(data)$variable.labels[81] <- "[Januar 2023] [Kontakte der Haushaltsmitglieder im Zusammenhang mit Schule/Kita] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[81] <- "Z1Q2_SQ004_SQ003"
# LimeSurvey Field type: F
data[, 82] <- as.numeric(data[, 82])
attributes(data)$variable.labels[82] <- "[Januar 2023] [Kontakte der Haushaltsmitglieder in der Freizeit] Nun geht es um die gesamten Kontakte aller Ihrer Haushaltsmitglieder (ohne Sie) in einer durchschnittlichen Woche im jeweiligen Zeitraum.  Wenn Sie beispielsweise mit Ihrem Partner und einem Kind zusammen wohnen, gehen Sie wie folgt vor für eine typische Woche vor der Pandemie:Schätzen Sie, wie viele berufliche Kontakte Ihr Partner in einer typischen Arbeitswoche hatte.Tragen Sie dies in das Feld \"In der Zeit vor der Pandemie / Kontakte im Zusammenhang mit Arbeit/Uni\" ein.  Schätzen Sie, wie viele Kontakte sowohl Ihr Kind als auch Ihr Partner (z.B. beim Abholen Ihres Kindes) im Zusammenhang mit Schule in einer typischen Woche hatte.Addieren Sie die Zahlen auf und tragen Sie diese in das Feld \"In der Zeit vor der Pandemie / Kontakte im Schule/Kita\" ein.  Gehen Sie für die weiteren Felder entsprechend vor.   "
names(data)[82] <- "Z1Q2_SQ004_SQ004"
# LimeSurvey Field type: F
data[, 83] <- as.numeric(data[, 83])
attributes(data)$variable.labels[83] <- "Wurde eines Ihrer Haushaltsmitglieder jemals positiv auf das Coronavirus (SARS-CoV-2/ COVID 19) getestet?"
data[, 83] <- factor(data[, 83], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[83] <- "BN2"
# LimeSurvey Field type: F
data[, 84] <- as.numeric(data[, 84])
attributes(data)$variable.labels[84] <- "[Im eigenen Haushalt] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 84] <- factor(data[, 84], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[84] <- "BN3_SQ001"
# LimeSurvey Field type: F
data[, 85] <- as.numeric(data[, 85])
attributes(data)$variable.labels[85] <- "[Auf der Arbeit] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 85] <- factor(data[, 85], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[85] <- "BN3_SQ002"
# LimeSurvey Field type: F
data[, 86] <- as.numeric(data[, 86])
attributes(data)$variable.labels[86] <- "[In der Schule] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 86] <- factor(data[, 86], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[86] <- "BN3_SQ003"
# LimeSurvey Field type: F
data[, 87] <- as.numeric(data[, 87])
attributes(data)$variable.labels[87] <- "[Treffen mit Freunden oder Bekannten] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 87] <- factor(data[, 87], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[87] <- "BN3_SQ004"
# LimeSurvey Field type: F
data[, 88] <- as.numeric(data[, 88])
attributes(data)$variable.labels[88] <- "[Sport / Freizeitveranstaltung] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 88] <- factor(data[, 88], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[88] <- "BN3_SQ005"
# LimeSurvey Field type: F
data[, 89] <- as.numeric(data[, 89])
attributes(data)$variable.labels[89] <- "[Einkaufen] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 89] <- factor(data[, 89], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[89] <- "BN3_SQ007"
# LimeSurvey Field type: F
data[, 90] <- as.numeric(data[, 90])
attributes(data)$variable.labels[90] <- "[Große Feier / Party] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 90] <- factor(data[, 90], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[90] <- "BN3_SQ008"
# LimeSurvey Field type: F
data[, 91] <- as.numeric(data[, 91])
attributes(data)$variable.labels[91] <- "[Konferenz / Workshop] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 91] <- factor(data[, 91], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[91] <- "BN3_SQ009"
# LimeSurvey Field type: F
data[, 92] <- as.numeric(data[, 92])
attributes(data)$variable.labels[92] <- "[Arztpraxis / Wartezimmer / Krankenhaus] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 92] <- factor(data[, 92], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[92] <- "BN3_SQ012"
# LimeSurvey Field type: F
data[, 93] <- as.numeric(data[, 93])
attributes(data)$variable.labels[93] <- "[Wohnheim (zum Beispiel Pflegewohnheim)] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 93] <- factor(data[, 93], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[93] <- "BN3_SQ013"
# LimeSurvey Field type: F
data[, 94] <- as.numeric(data[, 94])
attributes(data)$variable.labels[94] <- "[Kinderbetreuung] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 94] <- factor(data[, 94], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[94] <- "BN3_SQ014"
# LimeSurvey Field type: F
data[, 95] <- as.numeric(data[, 95])
attributes(data)$variable.labels[95] <- "[Urlaub] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 95] <- factor(data[, 95], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[95] <- "BN3_SQ015"
# LimeSurvey Field type: F
data[, 96] <- as.numeric(data[, 96])
attributes(data)$variable.labels[96] <- "[Im Nah- oder Fernverkehr] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 96] <- factor(data[, 96], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[96] <- "BN3_SQ016"
# LimeSurvey Field type: F
data[, 97] <- as.numeric(data[, 97])
attributes(data)$variable.labels[97] <- "[Restaurants / Bars ] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 97] <- factor(data[, 97], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[97] <- "BN3_SQ017"
# LimeSurvey Field type: F
data[, 98] <- as.numeric(data[, 98])
attributes(data)$variable.labels[98] <- "[Unbekannt] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 98] <- factor(data[, 98], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[98] <- "BN3_SQ010"
# LimeSurvey Field type: F
data[, 99] <- as.numeric(data[, 99])
attributes(data)$variable.labels[99] <- "[Keine Angabe] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
data[, 99] <- factor(data[, 99], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[99] <- "BN3_SQ011"
# LimeSurvey Field type: A
data[, 100] <- as.character(data[, 100])
attributes(data)$variable.labels[100] <- "[Sonstiges] Wo haben sich Ihre Haushaltsmitglieder angesteckt?"
names(data)[100] <- "BN3_other"
# LimeSurvey Field type: F
data[, 101] <- as.numeric(data[, 101])
attributes(data)$variable.labels[101] <- "[Im Jahr 2019] [Anzahl Personen, die im Haushalt gelebt haben.] Nun geht es um die Kontakte Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie. Ein engster Kontakt könnte ein:e Freund:in, ein:e Nachbar:in, ein:e Arbeitskollege:in, ein:e Pfleger:in oder ein Familienmitglied, mit dem Sie nicht in einem Haushalt wohnen, sein. Achten Sie bitte darauf, dass Sie sich auf einen einzigen Kontakt festlegen und diesen als Referenz für alle Fragen beibehalten. Das heißt, wenn Sie in etwa gleich viel Kontakt zu ihrer Schwester und ihrem besten Freund hatten, wählen Sie eine beliebige der beiden Personen aus und beantworten Sie die Fragen aus der Sicht dieser Person.  Wieviele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie gelebt?"
names(data)[101] <- "Z1Q31_SQ001_SQ001"
# LimeSurvey Field type: F
data[, 102] <- as.numeric(data[, 102])
attributes(data)$variable.labels[102] <- "[Ende März 2020] [Anzahl Personen, die im Haushalt gelebt haben.] Nun geht es um die Kontakte Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie. Ein engster Kontakt könnte ein:e Freund:in, ein:e Nachbar:in, ein:e Arbeitskollege:in, ein:e Pfleger:in oder ein Familienmitglied, mit dem Sie nicht in einem Haushalt wohnen, sein. Achten Sie bitte darauf, dass Sie sich auf einen einzigen Kontakt festlegen und diesen als Referenz für alle Fragen beibehalten. Das heißt, wenn Sie in etwa gleich viel Kontakt zu ihrer Schwester und ihrem besten Freund hatten, wählen Sie eine beliebige der beiden Personen aus und beantworten Sie die Fragen aus der Sicht dieser Person.  Wieviele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie gelebt?"
names(data)[102] <- "Z1Q31_SQ002_SQ001"
# LimeSurvey Field type: F
data[, 103] <- as.numeric(data[, 103])
attributes(data)$variable.labels[103] <- "[Im Sommer 2021] [Anzahl Personen, die im Haushalt gelebt haben.] Nun geht es um die Kontakte Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie. Ein engster Kontakt könnte ein:e Freund:in, ein:e Nachbar:in, ein:e Arbeitskollege:in, ein:e Pfleger:in oder ein Familienmitglied, mit dem Sie nicht in einem Haushalt wohnen, sein. Achten Sie bitte darauf, dass Sie sich auf einen einzigen Kontakt festlegen und diesen als Referenz für alle Fragen beibehalten. Das heißt, wenn Sie in etwa gleich viel Kontakt zu ihrer Schwester und ihrem besten Freund hatten, wählen Sie eine beliebige der beiden Personen aus und beantworten Sie die Fragen aus der Sicht dieser Person.  Wieviele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie gelebt?"
names(data)[103] <- "Z1Q31_SQ003_SQ001"
# LimeSurvey Field type: F
data[, 104] <- as.numeric(data[, 104])
attributes(data)$variable.labels[104] <- "[Januar 2023] [Anzahl Personen, die im Haushalt gelebt haben.] Nun geht es um die Kontakte Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie. Ein engster Kontakt könnte ein:e Freund:in, ein:e Nachbar:in, ein:e Arbeitskollege:in, ein:e Pfleger:in oder ein Familienmitglied, mit dem Sie nicht in einem Haushalt wohnen, sein. Achten Sie bitte darauf, dass Sie sich auf einen einzigen Kontakt festlegen und diesen als Referenz für alle Fragen beibehalten. Das heißt, wenn Sie in etwa gleich viel Kontakt zu ihrer Schwester und ihrem besten Freund hatten, wählen Sie eine beliebige der beiden Personen aus und beantworten Sie die Fragen aus der Sicht dieser Person.  Wieviele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie gelebt?"
names(data)[104] <- "Z1Q31_SQ004_SQ001"
# LimeSurvey Field type: F
data[, 105] <- as.numeric(data[, 105])
attributes(data)$variable.labels[105] <- "[Im Jahr 2019] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[105] <- "Z1Q3_SQ001_SQ002"
# LimeSurvey Field type: F
data[, 106] <- as.numeric(data[, 106])
attributes(data)$variable.labels[106] <- "[Im Jahr 2019] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[106] <- "Z1Q3_SQ001_SQ003"
# LimeSurvey Field type: F
data[, 107] <- as.numeric(data[, 107])
attributes(data)$variable.labels[107] <- "[Im Jahr 2019] [Kontakte dieses Kontakts in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[107] <- "Z1Q3_SQ001_SQ004"
# LimeSurvey Field type: F
data[, 108] <- as.numeric(data[, 108])
attributes(data)$variable.labels[108] <- "[Ende März 2020] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[108] <- "Z1Q3_SQ002_SQ002"
# LimeSurvey Field type: F
data[, 109] <- as.numeric(data[, 109])
attributes(data)$variable.labels[109] <- "[Ende März 2020] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[109] <- "Z1Q3_SQ002_SQ003"
# LimeSurvey Field type: F
data[, 110] <- as.numeric(data[, 110])
attributes(data)$variable.labels[110] <- "[Ende März 2020] [Kontakte dieses Kontakts in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[110] <- "Z1Q3_SQ002_SQ004"
# LimeSurvey Field type: F
data[, 111] <- as.numeric(data[, 111])
attributes(data)$variable.labels[111] <- "[Im Sommer 2021] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[111] <- "Z1Q3_SQ003_SQ002"
# LimeSurvey Field type: F
data[, 112] <- as.numeric(data[, 112])
attributes(data)$variable.labels[112] <- "[Im Sommer 2021] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[112] <- "Z1Q3_SQ003_SQ003"
# LimeSurvey Field type: F
data[, 113] <- as.numeric(data[, 113])
attributes(data)$variable.labels[113] <- "[Im Sommer 2021] [Kontakte dieses Kontakts in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[113] <- "Z1Q3_SQ003_SQ004"
# LimeSurvey Field type: F
data[, 114] <- as.numeric(data[, 114])
attributes(data)$variable.labels[114] <- "[Januar 2023] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[114] <- "Z1Q3_SQ004_SQ002"
# LimeSurvey Field type: F
data[, 115] <- as.numeric(data[, 115])
attributes(data)$variable.labels[115] <- "[Januar 2023] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[115] <- "Z1Q3_SQ004_SQ003"
# LimeSurvey Field type: F
data[, 116] <- as.numeric(data[, 116])
attributes(data)$variable.labels[116] <- "[Januar 2023] [Kontakte dieses Kontakts in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?  Falls es Ihnen schwerfällt, die Kontakte einzuschätzen, probieren Sie sich selbst als Vergleichswert vorzustellen, beispielsweise: \"Hatte meine Schwester/mein bester Freund mehr Kontakte bei der Arbeit? Wenn ja/nein, wie groß ist der Unterschied in etwa?\"."
names(data)[116] <- "Z1Q3_SQ004_SQ004"
# LimeSurvey Field type: F
data[, 117] <- as.numeric(data[, 117])
attributes(data)$variable.labels[117] <- "Hat sich die Person, mit der Sie den engsten Kontakt (Nicht-Haushaltsmitglied) vor der COVID-Pandemie hatten, während der Pandemie geändert?"
data[, 117] <- factor(data[, 117], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[117] <- "Z1Q4"
# LimeSurvey Field type: F
data[, 118] <- as.numeric(data[, 118])
attributes(data)$variable.labels[118] <- "[Im Jahr 2019] [Anzahl Personen die im Haushalt leben] Sie haben angegeben, dass sich ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie geändert hat. Daher möchten wir Sie nun bitten, sich in die Situation Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu begeben.  Wie viele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit während der COVID-Pandemie gelebt?   "
names(data)[118] <- "Z1Q4Y1_SQ001_SQ001"
# LimeSurvey Field type: F
data[, 119] <- as.numeric(data[, 119])
attributes(data)$variable.labels[119] <- "[Ende März 2020] [Anzahl Personen die im Haushalt leben] Sie haben angegeben, dass sich ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie geändert hat. Daher möchten wir Sie nun bitten, sich in die Situation Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu begeben.  Wie viele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit während der COVID-Pandemie gelebt?   "
names(data)[119] <- "Z1Q4Y1_SQ002_SQ001"
# LimeSurvey Field type: F
data[, 120] <- as.numeric(data[, 120])
attributes(data)$variable.labels[120] <- "[Im Sommer 2021] [Anzahl Personen die im Haushalt leben] Sie haben angegeben, dass sich ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie geändert hat. Daher möchten wir Sie nun bitten, sich in die Situation Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu begeben.  Wie viele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit während der COVID-Pandemie gelebt?   "
names(data)[120] <- "Z1Q4Y1_SQ003_SQ001"
# LimeSurvey Field type: F
data[, 121] <- as.numeric(data[, 121])
attributes(data)$variable.labels[121] <- "[Januar 2023] [Anzahl Personen die im Haushalt leben] Sie haben angegeben, dass sich ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie geändert hat. Daher möchten wir Sie nun bitten, sich in die Situation Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu begeben.  Wie viele Personen haben in den folgenden Zeiträumen in dem Haushalt Ihres engsten Kontaktes (Nicht-Haushaltsmitglied) aus der Zeit während der COVID-Pandemie gelebt?   "
names(data)[121] <- "Z1Q4Y1_SQ004_SQ001"
# LimeSurvey Field type: F
data[, 122] <- as.numeric(data[, 122])
attributes(data)$variable.labels[122] <- "[Im Jahr 2019] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[122] <- "Z1Q4Y_SQ001_SQ002"
# LimeSurvey Field type: F
data[, 123] <- as.numeric(data[, 123])
attributes(data)$variable.labels[123] <- "[Im Jahr 2019] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[123] <- "Z1Q4Y_SQ001_SQ003"
# LimeSurvey Field type: F
data[, 124] <- as.numeric(data[, 124])
attributes(data)$variable.labels[124] <- "[Im Jahr 2019] [Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[124] <- "Z1Q4Y_SQ001_SQ004"
# LimeSurvey Field type: F
data[, 125] <- as.numeric(data[, 125])
attributes(data)$variable.labels[125] <- "[Ende März 2020] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[125] <- "Z1Q4Y_SQ002_SQ002"
# LimeSurvey Field type: F
data[, 126] <- as.numeric(data[, 126])
attributes(data)$variable.labels[126] <- "[Ende März 2020] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[126] <- "Z1Q4Y_SQ002_SQ003"
# LimeSurvey Field type: F
data[, 127] <- as.numeric(data[, 127])
attributes(data)$variable.labels[127] <- "[Ende März 2020] [Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[127] <- "Z1Q4Y_SQ002_SQ004"
# LimeSurvey Field type: F
data[, 128] <- as.numeric(data[, 128])
attributes(data)$variable.labels[128] <- "[Im Sommer 2021] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[128] <- "Z1Q4Y_SQ003_SQ002"
# LimeSurvey Field type: F
data[, 129] <- as.numeric(data[, 129])
attributes(data)$variable.labels[129] <- "[Im Sommer 2021] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[129] <- "Z1Q4Y_SQ003_SQ003"
# LimeSurvey Field type: F
data[, 130] <- as.numeric(data[, 130])
attributes(data)$variable.labels[130] <- "[Im Sommer 2021] [Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[130] <- "Z1Q4Y_SQ003_SQ004"
# LimeSurvey Field type: F
data[, 131] <- as.numeric(data[, 131])
attributes(data)$variable.labels[131] <- "[Januar 2023] [Kontakte dieses Kontakts im Zusammenhang mit Arbeit/Uni] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[131] <- "Z1Q4Y_SQ004_SQ002"
# LimeSurvey Field type: F
data[, 132] <- as.numeric(data[, 132])
attributes(data)$variable.labels[132] <- "[Januar 2023] [Kontakte dieses Kontakts im Zusammenhang mit Schule/Kita] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[132] <- "Z1Q4Y_SQ004_SQ003"
# LimeSurvey Field type: F
data[, 133] <- as.numeric(data[, 133])
attributes(data)$variable.labels[133] <- "[Januar 2023] [Kontakte in der Freizeit] In einer durchschnittlichen Woche, wie viele Kontakte hatte Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie zu den folgenden Zeiträumen in den jeweiligen Kontexten?   "
names(data)[133] <- "Z1Q4Y_SQ004_SQ004"
# LimeSurvey Field type: F
data[, 134] <- as.numeric(data[, 134])
attributes(data)$variable.labels[134] <- "Haben Sie zu einem der genannten Zeitpunkte ihren Kontakt zu Personen reduziert und zwar aufgrund einer Differenz in Ihrer Meinung zu COVID-19?"
data[, 134] <- factor(data[, 134], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[134] <- "Z3Q1"
# LimeSurvey Field type: F
data[, 135] <- as.numeric(data[, 135])
attributes(data)$variable.labels[135] <- "Zu wie vielen Personen haben Sie den Kontakt reduziert, und zwar aufgrund einer Differenz in Ihrer Meinung zu COVID-19?"
names(data)[135] <- "Z3Q1O1"
# LimeSurvey Field type: A
data[, 136] <- as.character(data[, 136])
attributes(data)$variable.labels[136] <- "Gerne können Sie Ihre Antwort hier erläutern. Wir sind insbesondere an den Gründen interessiert, warum und in welchem Zusammenhang sich Kontakte ändern oder eben nicht."
names(data)[136] <- "Z3Q1O2"
# LimeSurvey Field type: F
data[, 137] <- as.numeric(data[, 137])
attributes(data)$variable.labels[137] <- "Haben Sie zu einem der genannten Zeitpunkte einen engen Kontakt hergestellt oder intensiviert, und zwar aufgrund gleicher Einstellung zu COVID-19?"
data[, 137] <- factor(data[, 137], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[137] <- "Z4Q1"
# LimeSurvey Field type: F
data[, 138] <- as.numeric(data[, 138])
attributes(data)$variable.labels[138] <- "Wie viele Kontakte haben Sie aufgebaut oder intensiviert, und zwar aufgrund gleicher Einstellung zu COVID-19?"
names(data)[138] <- "Z4Q1O1"
# LimeSurvey Field type: A
data[, 139] <- as.character(data[, 139])
attributes(data)$variable.labels[139] <- "Gerne können Sie Ihre Antwort hier erläutern."
names(data)[139] <- "Z4Q1O2"
# LimeSurvey Field type: A
data[, 140] <- as.character(data[, 140])
attributes(data)$variable.labels[140] <- "[dass ich Kontakte in Präsenz vermeide. ] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 140] <- factor(data[, 140], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[140] <- "RISK1_SQ001"
# LimeSurvey Field type: A
data[, 141] <- as.character(data[, 141])
attributes(data)$variable.labels[141] <- "[dass ich Kontakte zu Personen vermeide, die unvorsichtig sind.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 141] <- factor(data[, 141], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[141] <- "RISK1_SQ017"
# LimeSurvey Field type: A
data[, 142] <- as.character(data[, 142])
attributes(data)$variable.labels[142] <- "[dass ich Kontakt nur zu Personen habe, die ähnlich vorsichtig sind wie ich.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 142] <- factor(data[, 142], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[142] <- "RISK1_SQ016"
# LimeSurvey Field type: A
data[, 143] <- as.character(data[, 143])
attributes(data)$variable.labels[143] <- "[dass ich nicht zu Stoßzeiten einkaufen gehe.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 143] <- factor(data[, 143], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[143] <- "RISK1_SQ012"
# LimeSurvey Field type: A
data[, 144] <- as.character(data[, 144])
attributes(data)$variable.labels[144] <- "[dass ich mindestens 1,5 Meter Abstand zu anderen Personen halte.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 144] <- factor(data[, 144], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[144] <- "RISK1_SQ011"
# LimeSurvey Field type: A
data[, 145] <- as.character(data[, 145])
attributes(data)$variable.labels[145] <- "[dass Kontakte nur draußen geschehen.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 145] <- factor(data[, 145], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[145] <- "RISK1_SQ002"
# LimeSurvey Field type: A
data[, 146] <- as.character(data[, 146])
attributes(data)$variable.labels[146] <- "[dass ich keine Risikopersonen besuche.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 146] <- factor(data[, 146], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[146] <- "RISK1_SQ006"
# LimeSurvey Field type: A
data[, 147] <- as.character(data[, 147])
attributes(data)$variable.labels[147] <- "[dass ich in meiner Freizeit öffentliche Orte mit vielen Personen meide.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 147] <- factor(data[, 147], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[147] <- "RISK1_SQ007"
# LimeSurvey Field type: A
data[, 148] <- as.character(data[, 148])
attributes(data)$variable.labels[148] <- "[dass ich öffentlichen Nahverkehr grundsätzlich vermeide. ] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 148] <- factor(data[, 148], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[148] <- "RISK1_SQ009"
# LimeSurvey Field type: A
data[, 149] <- as.character(data[, 149])
attributes(data)$variable.labels[149] <- "[dass ich im öffentlichen Nahverkehr stets eine Maske getragen habe.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 149] <- factor(data[, 149], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[149] <- "RISK1_SQ003"
# LimeSurvey Field type: A
data[, 150] <- as.character(data[, 150])
attributes(data)$variable.labels[150] <- "[dass ich im Supermarkt stets eine Maske getragen habe.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 150] <- factor(data[, 150], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[150] <- "RISK1_SQ004"
# LimeSurvey Field type: A
data[, 151] <- as.character(data[, 151])
attributes(data)$variable.labels[151] <- "[dass ich nur im Home-Office arbeite.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 151] <- factor(data[, 151], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[151] <- "RISK1_SQ008"
# LimeSurvey Field type: A
data[, 152] <- as.character(data[, 152])
attributes(data)$variable.labels[152] <- "[dass meine Kinder nur mit ausgewählten Personen Kontakt haben.] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 152] <- factor(data[, 152], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[152] <- "RISK1_SQ013"
# LimeSurvey Field type: A
data[, 153] <- as.character(data[, 153])
attributes(data)$variable.labels[153] <- "[dass ich mir nahestehende Personen getroffen habe, auch trotz Verbot. ] Vielen Dank, dass Sie die vorherigen Fragen beantwortet haben. Jetzt wollen wir von Ihnen wissen, wie Sie im Verlaufe der Pandemie mit verschiedenen Situationen umgegangen sind. Hierbei beziehen wir uns auf dieselben Zeitpunkte wie in den vorherigen Fragen.     Zuerst wollen wir etwas zum Ende März 2020, also dem Beginn der Pandemie, von Ihnen erfahren.  Wie haben Sie sich in folgenden Situationen verhalten?  Im Vergleich zu einer durchschnittlichen Person habe ich darauf geachtet, ..."
data[, 153] <- factor(data[, 153], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[153] <- "RISK1_SQ015R"
# LimeSurvey Field type: A
data[, 154] <- as.character(data[, 154])
attributes(data)$variable.labels[154] <- "[habe ich das Ansteckungsrisiko als gering eingeschätzt.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 154] <- factor(data[, 154], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[154] <- "RISK1A_SQ018"
# LimeSurvey Field type: A
data[, 155] <- as.character(data[, 155])
attributes(data)$variable.labels[155] <- "[habe ich den Verlauf einer Infektion als risikoreich eingeschätzt.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 155] <- factor(data[, 155], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[155] <- "RISK1A_SQ019"
# LimeSurvey Field type: A
data[, 156] <- as.character(data[, 156])
attributes(data)$variable.labels[156] <- "[hatte ich eine hohe Risikowahrnehmung.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 156] <- factor(data[, 156], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[156] <- "RISK1A_SQ020"
# LimeSurvey Field type: A
data[, 157] <- as.character(data[, 157])
attributes(data)$variable.labels[157] <- "[habe ich häufig risikoreiche Situationen vermieden.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 157] <- factor(data[, 157], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[157] <- "RISK1A_SQ021"
# LimeSurvey Field type: A
data[, 158] <- as.character(data[, 158])
attributes(data)$variable.labels[158] <- "[war ich mir der Effektivität von Abstandsregeln bewusst.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 158] <- factor(data[, 158], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[158] <- "RISK1A_SQ022"
# LimeSurvey Field type: A
data[, 159] <- as.character(data[, 159])
attributes(data)$variable.labels[159] <- "[war mir klar, dass korrektes Masketragen das Infektionsrisiko senkt.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 159] <- factor(data[, 159], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[159] <- "RISK1A_SQ023"
# LimeSurvey Field type: A
data[, 160] <- as.character(data[, 160])
attributes(data)$variable.labels[160] <- "[konnte ich den empfohlenen Maßnahmen Folge leisten.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 160] <- factor(data[, 160], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[160] <- "RISK1A_SQ024"
# LimeSurvey Field type: A
data[, 161] <- as.character(data[, 161])
attributes(data)$variable.labels[161] <- "[habe ich mich durch die vorgeschriebenen Maßnahmen eingeschränkt gefühlt.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 161] <- factor(data[, 161], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[161] <- "RISK1A_SQ025"
# LimeSurvey Field type: A
data[, 162] <- as.character(data[, 162])
attributes(data)$variable.labels[162] <- "[habe ich FFP2/3 Maske anstatt medizinischer Maske getragen.] Auch die folgenden Fragen beziehen sich auf Ende März 2020, also dem Beginn der Pandemie.  Wie stehen Sie zu den folgenden Aussagen?  Im Vergleich zu einer durchschnittlichen Person..."
data[, 162] <- factor(data[, 162], levels=c("RA7","RA6","RA5","RA4","RA3","RA2","RA1","RA8","RA0"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "trifft nicht zu", "keine Angabe"))
names(data)[162] <- "RISK1A_SQ026"
# LimeSurvey Field type: A
data[, 163] <- as.character(data[, 163])
attributes(data)$variable.labels[163] <- "[Im Zeitraum März 2020 war ich im Vergleich zum Sommer 2021 ... vorsichtig.] Wie würden Sie die Veränderung Ihres Vorsichtsverhaltens im Vergleich zur durchschnittlichen Person während der Pandemie beschreiben?"
data[, 163] <- factor(data[, 163], levels=c("A1","A100","A2","A3","A4","A5","A101","A99"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "keine Angabe"))
names(data)[163] <- "RISK2Change_SQ2021"
# LimeSurvey Field type: A
data[, 164] <- as.character(data[, 164])
attributes(data)$variable.labels[164] <- "[Im Zeitraum Sommer 2021 war ich im Vergleich zum Januar 2023 ... vorsichtig.] Wie würden Sie die Veränderung Ihres Vorsichtsverhaltens im Vergleich zur durchschnittlichen Person während der Pandemie beschreiben?"
data[, 164] <- factor(data[, 164], levels=c("A1","A100","A2","A3","A4","A5","A101","A99"),labels=c("viel weniger", "weniger", "etwas weniger", "genauso", "etwas mehr", "mehr", "viel mehr", "keine Angabe"))
names(data)[164] <- "RISK2Change_SQ2022"
# LimeSurvey Field type: A
data[, 165] <- as.character(data[, 165])
attributes(data)$variable.labels[165] <- "Haben Sie bereits Impfungen gegen COVID-19 bekommen?"
data[, 165] <- factor(data[, 165], levels=c("A1","A2","A3","A4"),labels=c("Ja", "Nein", "Weiß ich nicht", "Ich möchte nicht antworten"))
names(data)[165] <- "C1"
# LimeSurvey Field type: A
data[, 166] <- as.character(data[, 166])
attributes(data)$variable.labels[166] <- "[erste Impfung] Wenn ja, wann wurden Sie geimpft, wie viele Impfungen haben Sie bekommen und welcher Impfstoff wurde verwendet?"
data[, 166] <- factor(data[, 166], levels=c("A1","A2","A3","A4","A5","A6","A8","A7"),labels=c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "AstraZeneca", "Andere", "Nicht zutreffend", "Ich möchte nicht antworten"))
names(data)[166] <- "C1S1_SQ001"
# LimeSurvey Field type: A
data[, 167] <- as.character(data[, 167])
attributes(data)$variable.labels[167] <- "[zweite Impfung] Wenn ja, wann wurden Sie geimpft, wie viele Impfungen haben Sie bekommen und welcher Impfstoff wurde verwendet?"
data[, 167] <- factor(data[, 167], levels=c("A1","A2","A3","A4","A5","A6","A8","A7"),labels=c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "AstraZeneca", "Andere", "Nicht zutreffend", "Ich möchte nicht antworten"))
names(data)[167] <- "C1S1_SQ002"
# LimeSurvey Field type: A
data[, 168] <- as.character(data[, 168])
attributes(data)$variable.labels[168] <- "[dritte Impfung (Booster)] Wenn ja, wann wurden Sie geimpft, wie viele Impfungen haben Sie bekommen und welcher Impfstoff wurde verwendet?"
data[, 168] <- factor(data[, 168], levels=c("A1","A2","A3","A4","A5","A6","A8","A7"),labels=c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "AstraZeneca", "Andere", "Nicht zutreffend", "Ich möchte nicht antworten"))
names(data)[168] <- "C1S1_SQ003"
# LimeSurvey Field type: A
data[, 169] <- as.character(data[, 169])
attributes(data)$variable.labels[169] <- "[vierte Impfung (Booster)] Wenn ja, wann wurden Sie geimpft, wie viele Impfungen haben Sie bekommen und welcher Impfstoff wurde verwendet?"
data[, 169] <- factor(data[, 169], levels=c("A1","A2","A3","A4","A5","A6","A8","A7"),labels=c("BioNTech", "Moderna", "Janssen/ Johnson & Johnson", "Gamaleya Sputnik V", "AstraZeneca", "Andere", "Nicht zutreffend", "Ich möchte nicht antworten"))
names(data)[169] <- "C1S1_SQ004"
# LimeSurvey Field type: DATETIME23.2
data[, 170] <- as.character(data[, 170])
attributes(data)$variable.labels[170] <- "Wenn ja, wann haben Sie die letzte Impfung bekommen?"
names(data)[170] <- "C1S2"
# LimeSurvey Field type: A
data[, 171] <- as.character(data[, 171])
attributes(data)$variable.labels[171] <- "Haben oder werden Sie sich dieses Jahr (erneut) gegen COVID-19 impfen lassen?"
data[, 171] <- factor(data[, 171], levels=c("A1","A4","A2","A3"),labels=c("Ja", "Vielleicht", "Auf keinen Fall", "Ich möchte nicht antworten"))
names(data)[171] <- "C13"
# LimeSurvey Field type: F
data[, 172] <- as.numeric(data[, 172])
attributes(data)$variable.labels[172] <- "[Weil es eine Empfehlung der STIKO (Ständige Impfkommission) dafür gibt.] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 172] <- factor(data[, 172], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[172] <- "C14_SQ001"
# LimeSurvey Field type: F
data[, 173] <- as.numeric(data[, 173])
attributes(data)$variable.labels[173] <- "[Aufgrund einer starken Infektionsausbreitung im kommenden Jahr.] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 173] <- factor(data[, 173], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[173] <- "C14_SQ005"
# LimeSurvey Field type: F
data[, 174] <- as.numeric(data[, 174])
attributes(data)$variable.labels[174] <- "[Weil ein neuer Impfstoff zugelassen wird.] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 174] <- factor(data[, 174], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[174] <- "C14_SQ004"
# LimeSurvey Field type: F
data[, 175] <- as.numeric(data[, 175])
attributes(data)$variable.labels[175] <- "[Weil es einen neuen Impfstoff gibt, der an neuere Varianten angepasst ist.] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 175] <- factor(data[, 175], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[175] <- "C14_SQ003"
# LimeSurvey Field type: F
data[, 176] <- as.numeric(data[, 176])
attributes(data)$variable.labels[176] <- "[Anderes] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 176] <- factor(data[, 176], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[176] <- "C14_SQ008"
# LimeSurvey Field type: F
data[, 177] <- as.numeric(data[, 177])
attributes(data)$variable.labels[177] <- "[Weiß ich nicht.] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 177] <- factor(data[, 177], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[177] <- "C14_SQ006"
# LimeSurvey Field type: F
data[, 178] <- as.numeric(data[, 178])
attributes(data)$variable.labels[178] <- "[Ich möchte nicht antworten.] Aus welchen Gründen würden Sie sich in diesem Jahr für eine COVID-19 Impfung entscheiden oder haben dies bereits getan? (Mehrfachauswahl)"
data[, 178] <- factor(data[, 178], levels=c(1,0),labels=c("Ja", "Nicht Gewählt"))
names(data)[178] <- "C14_SQ007"
# LimeSurvey Field type: A
data[, 179] <- as.character(data[, 179])
attributes(data)$variable.labels[179] <- "Würden Sie sich in diesem Jahr auf Empfehlung Ihres Hausarztes/Ihrer Hausärztin gegen eine andere Krankheit außer COVID-19 (z.B. Influenza, Pneumokokken, Tetanus, HPV, Herpes Zoster, Hepatitis A/B, Masern) impfen lassen oder haben Sie dieses bereits getan?"
data[, 179] <- factor(data[, 179], levels=c("A1","A4","A2","A3"),labels=c("Ja", "Vielleicht", "Auf keinen Fall", "Ich möchte nicht antworten"))
names(data)[179] <- "C10"
# LimeSurvey Field type: A
data[, 180] <- as.character(data[, 180])
attributes(data)$variable.labels[180] <- "[Tetanus] Wie wichtig finden Sie es, gegen die folgenden Erkrankungen geimpft zu sein, unabhängig von Ihrem Impfstatus?"
data[, 180] <- factor(data[, 180], levels=c("A1","A2","A3","A4","A5"),labels=c("Nicht so  wichtig", "Wichtig", "Besonders wichtig", "Ich weiß es nicht", "Ich möchte nicht antworten"))
names(data)[180] <- "C9_SQ001"
# LimeSurvey Field type: A
data[, 181] <- as.character(data[, 181])
attributes(data)$variable.labels[181] <- "[Influenza (Grippe) ] Wie wichtig finden Sie es, gegen die folgenden Erkrankungen geimpft zu sein, unabhängig von Ihrem Impfstatus?"
data[, 181] <- factor(data[, 181], levels=c("A1","A2","A3","A4","A5"),labels=c("Nicht so  wichtig", "Wichtig", "Besonders wichtig", "Ich weiß es nicht", "Ich möchte nicht antworten"))
names(data)[181] <- "C9_SQ002"
# LimeSurvey Field type: A
data[, 182] <- as.character(data[, 182])
attributes(data)$variable.labels[182] <- "[Pneumokokken (Lungenentzündung)] Wie wichtig finden Sie es, gegen die folgenden Erkrankungen geimpft zu sein, unabhängig von Ihrem Impfstatus?"
data[, 182] <- factor(data[, 182], levels=c("A1","A2","A3","A4","A5"),labels=c("Nicht so  wichtig", "Wichtig", "Besonders wichtig", "Ich weiß es nicht", "Ich möchte nicht antworten"))
names(data)[182] <- "C9_SQ003"
# LimeSurvey Field type: A
data[, 183] <- as.character(data[, 183])
attributes(data)$variable.labels[183] <- "[Masern] Wie wichtig finden Sie es, gegen die folgenden Erkrankungen geimpft zu sein, unabhängig von Ihrem Impfstatus?"
data[, 183] <- factor(data[, 183], levels=c("A1","A2","A3","A4","A5"),labels=c("Nicht so  wichtig", "Wichtig", "Besonders wichtig", "Ich weiß es nicht", "Ich möchte nicht antworten"))
names(data)[183] <- "C9_SQ004"
# LimeSurvey Field type: A
data[, 184] <- as.character(data[, 184])
attributes(data)$variable.labels[184] <- "[COVID-19] Wie wichtig finden Sie es, gegen die folgenden Erkrankungen geimpft zu sein, unabhängig von Ihrem Impfstatus?"
data[, 184] <- factor(data[, 184], levels=c("A1","A2","A3","A4","A5"),labels=c("Nicht so  wichtig", "Wichtig", "Besonders wichtig", "Ich weiß es nicht", "Ich möchte nicht antworten"))
names(data)[184] <- "C9_SQ005"
# LimeSurvey Field type: A
data[, 185] <- as.character(data[, 185])
attributes(data)$variable.labels[185] <- "Bitte geben Sie Ihren höchsten Bildungsabschluss an."
data[, 185] <- factor(data[, 185], levels=c("A1","A2","A3","A5","A4","A6"),labels=c("Keinen Schulabschluss", "Haupt-/ Volksschulabschluss", "Realschulabschluss", "Abitur / Fachhochschulabitur", "Anderer", "Ich möchte nicht antworten"))
names(data)[185] <- "A6"
# LimeSurvey Field type: A
data[, 186] <- as.character(data[, 186])
attributes(data)$variable.labels[186] <- "Was machen Sie aktuell beruflich?"
data[, 186] <- factor(data[, 186], levels=c("A1","A2","A3","A4","A5","A6","A7","A8"),labels=c("Ich bin arbeitssuchend.", "Ich bin Rentner:in oder Pensionär:in.", "Ich bin im Studium oder in der Ausbildung.", "Ich bin in einem medizinischen oder pflegerischen Beruf bei einem Gesundheitsversorger tätig.", "Ich bin als Lehrer:in oder Erzieher:in tätig.", "Ich bin in einem anderen Beruf tätig.", "Andere (z.B. Elternzeit, Sabbatical)", "Ich möchte nicht antworten"))
names(data)[186] <- "A7"
# LimeSurvey Field type: A
data[, 187] <- as.character(data[, 187])
attributes(data)$variable.labels[187] <- "Rauchen Sie?"
data[, 187] <- factor(data[, 187], levels=c("A1","A4","A3","A2","A5"),labels=c("Ja, täglich", "Ja, gelegentlich", "Nein, nicht mehr", "Ich habe noch nie geraucht.", "Ich möchte nicht antworten"))
names(data)[187] <- "A10"
# LimeSurvey Field type: A
data[, 188] <- as.character(data[, 188])
attributes(data)$variable.labels[188] <- "Haben Sie Kinder?"
data[, 188] <- factor(data[, 188], levels=c("A1","A2","A3"),labels=c("Ja", "Nein", "Ich möchte nicht antworten"))
names(data)[188] <- "A11"
# LimeSurvey Field type: F
data[, 189] <- as.numeric(data[, 189])
attributes(data)$variable.labels[189] <- "Wie viele Kinder unter 18 Jahren haben Sie?"
names(data)[189] <- "A11S1"
# LimeSurvey Field type: F
data[, 190] <- as.numeric(data[, 190])
attributes(data)$variable.labels[190] <- "[Personenanzahl unter 14 Jahre ] Bitte geben Sie die Anzahl der Personen an, die in Ihrem Haushalt wohnen (Sie selbst eingeschlossen)."
names(data)[190] <- "A20_SQ001"
# LimeSurvey Field type: F
data[, 191] <- as.numeric(data[, 191])
attributes(data)$variable.labels[191] <- "[Personenanzahl 14 Jahre und älter] Bitte geben Sie die Anzahl der Personen an, die in Ihrem Haushalt wohnen (Sie selbst eingeschlossen)."
names(data)[191] <- "A20_SQ002"
# LimeSurvey Field type: F
data[, 192] <- as.numeric(data[, 192])
attributes(data)$variable.labels[192] <- "Dürfen wir Sie bitten, diese Umfrage an Ihren engsten Kontakt (Nicht-Haushaltsmitglied) weiterzuleiten? Hierbei beziehen wir uns auf den Kontakt, den Sie als Referenz für die Beantwortung der vorangegangenen Kontaktfragen verwendet haben.  Falls sich Ihr engster Kontakt (Nicht-Haushaltsmitglied) im Laufe der COVID-Pandemie nicht geändert haben sollte, dann hatten Sie nur eine Referenzperson zur Beantwortung der Fragen: Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie. Bitte leiten Sie die Umfrage in diesem Falle an diese Person weiter.  Falls sich Ihr engster Kontakt (Nicht-Haushaltsmitglied) im Laufe der COVID-Pandemie geändert haben sollte, dann hatten Sie zwei Referenzpersonen zur Beantwortung der Fragen: 1. Ihr engster Kontakt (Nicht-Haushaltsmitglied) aus der Zeit vor der COVID-Pandemie und 2. Ihr engster Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie. Bitte leiten Sie die Umfrage in diesem Falle an letztere weiter, also Ihren engsten Kontakt (Nicht-Haushaltsmitglied) während der COVID-Pandemie.  Dies würde uns sehr weiterhelfen!"
data[, 192] <- factor(data[, 192], levels=c(1,2),labels=c("Ja", "Nein"))
names(data)[192] <- "ASKREF"
# LimeSurvey Field type: A
data[, 193] <- as.character(data[, 193])
attributes(data)$variable.labels[193] <- "Vielen Dank! Gerne können Sie diesen Link zum Teilen verwenden: -  Kopieren"
names(data)[193] <- "REFLINK"
# LimeSurvey Field type: A
data[, 194] <- as.character(data[, 194])
attributes(data)$variable.labels[194] <- "Sie können uns hier gerne einen Kommentar/Anregung/Anmerkung hinterlassen.  Bitte hinterlassen Sie hier keine Kontaktinformationen oder andere Informationen, die Sie als Person identifizieren könnten."
names(data)[194] <- "Kommentare"
