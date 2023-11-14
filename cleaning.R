#####################################################   CZYSZCZENIE   #######################################################
set.seed(72685)

##################### LADOWANIE DANYCH
patients <- read.csv("patients.csv", sep=',', header=TRUE,na.strings = c("", "NA"))
symptoms <- read.csv("symptoms.csv", sep=',', header=TRUE)
vaccines <- read.csv("vaccines.csv", sep=',', header=TRUE,na.strings = c("", "NA"))

##################### WYBOR KOLUMN
### VACCINES
vaccines <- subset(vaccines, select = -c(VAX_NAME, VAX_LOT))

### SYMPTOMS
library(tidyr)
symptoms <- subset(symptoms, select = -c(SYMPTOMVERSION1, SYMPTOMVERSION2, SYMPTOMVERSION3, SYMPTOMVERSION4, SYMPTOMVERSION5))
symptoms <- symptoms %>% unite(SYMPTOMS, SYMPTOM1, SYMPTOM2, SYMPTOM3, SYMPTOM4, SYMPTOM5, sep=",")
symptoms <- aggregate(data=symptoms,SYMPTOMS~.,FUN=paste,collapse=",")
symptoms <- unique(symptoms)

### PATIENTS
patients <- subset(patients, select = -c(RECVDATE, CAGE_YR, CAGE_MO, RPT_DATE, SYMPTOM_TEXT, DATEDIED, ER_VISIT, VAX_DATE,
                                         ONSET_DATE, LAB_DATA, V_FUNDBY, OTHER_MEDS, CUR_ILL, HISTORY, PRIOR_VAX, SPLTTYPE,
                                         FORM_VERS, TODAYS_DATE, BIRTH_DEFECT, ALLERGIES, STATE, DIED, X_STAY))

##################### LACZENIE ZBIOROW I TWORZENIE KOPII ROBOCZEJ
draft <- merge(x = vaccines, y = patients, by = "VAERS_ID", all.y = TRUE)
draft <- draft[draft$VAX_TYPE == "COVID19",]
draft <- subset(draft, select = -c(VAX_TYPE)) #vax_type - dluzej niepotrzebne bo mamy tylko covid
rm(patients, vaccines) #usuniecie niepotrzebnych juz tabel
draft <- subset(draft, select = -c(VAERS_ID)) #usuniecie ID, po polaczeniu jest bezuzyteczne
data <- draft

##################### CZYSZCZENIE I PRZEKODOWYWANIE
library(sjmisc)
library(gtools)
data
#RECOVD
data$RECOVD <- rec(data$RECOVD, rec = "Y=1;N=0;NA,U=-1")
data <- data[data$RECOVD != -1,]
data$RECOVD <- as.factor(data$RECOVD)
data$RECOVD <- droplevels(data$RECOVD)

#NUMDAYS
data <- data[data$NUMDAYS < 180,]
data$NUMDAYS <- rec(data$NUMDAYS, rec = "0=0;1=1;2=2;3=3;4,5,6,7=4-7;8,9,10,11,12,13,14=8-14;15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30=14-30;NA=UNK;else=30+")
#set zawiera zdarzenia do 09.07, zakładając że szczepienia są od 01.01.21 i liczac na okrągło, odrzucamy numdays wieksze niz 120

#HOSPDAYS
data$HOSPDAYS <- rec(data$HOSPDAYS, rec = "NA=0;else=copy")
data <- data[data$HOSPDAYS < 99999,]
#usuniecie 3 wierszy, wg ktorych osoba spedzila w szpitalu 10k dni

#VAX_DOSE_SERIES
data$VAX_DOSE_SERIES <- rec(data$VAX_DOSE_SERIES, rec = "3,4,5,6,7+,N/A,UNK=OT;else=copy")

#VAX_MANU
data$VAX_MANU <- rec(data$VAX_MANU, rec = "UNKNOWN MANUFACTURER=UNKNOWN;else=copy")
data$VAX_MANU <- as.factor(data$VAX_MANU)
data$VAX_MANU <- droplevels(data$VAX_MANU)


#VAX_ROUTE
data$VAX_ROUTE <- rec(data$VAX_ROUTE, rec = "ID,IN,JET,PO,SC,UN,NA=OT;else=copy")

#AGE_YRS
data$AGE_YRS <- quantcut(data$AGE_YRS, q=10, na.rm=TRUE)
data$AGE_YRS <- rec(data$AGE_YRS, rec = "NA=UNK;else=copy")

#L_THREAT
data$L_THREAT <- rec(data$L_THREAT, rec = "Y=1;NA=0")

#HOSPITAL
data$HOSPITAL <- rec(data$HOSPITAL, rec = "Y=1;NA=0")

#DISABLE
data$DISABLE <- rec(data$DISABLE, rec = "Y=1;NA=0")

#V_ADMINBY
data$V_ADMINBY <- rec(data$V_ADMINBY, rec = "NA=UNK;else=copy")

#OFC_VISIT
data$OFC_VISIT <- rec(data$OFC_VISIT, rec = "Y=1;NA=0")

#ER_ED_VISIT
data$ER_ED_VISIT <- rec(data$ER_ED_VISIT, rec = "Y=1;NA=0")

#VAX_SITE
data$VAX_SITE <- rec(data$VAX_SITE, rec = "AR,UN,GM,LG,LL,MO,NS,OT,RL,NA=OT;else=copy")

#PODSUMOWANIE CZYSZCZENIA
data <- data[complete.cases(data), ] #usuniecie 15k wierszy zawierających tylko NA
sum(is.na(data)) #dalsze braki danych nie wystepuja
data <- unique(data) #usuniecie powtarzajacych sie wierszy - z 292k na 111k
rm(draft)

##################### Zapisanie wyczyszczonych danych
#write.csv2(data, "data.csv", row.names = FALSE)
#write.csv2(symptoms, "symptoms_clean.csv", row.names = FALSE)
############################################################################################################################
