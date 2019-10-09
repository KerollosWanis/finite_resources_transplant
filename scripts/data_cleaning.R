#read in the raw data
cand_liin <- read.csv("CSV/cand_liin.csv", na.strings = "")
stathist_liin <- read.csv("CSV/stathist_liin.csv", na.strings = "")
tx_li <- read.csv("CSV/tx_li.csv", na.strings = "")
donor_deceased <- read.csv("CSV/donor_deceased.csv", na.strings="")

#clean the 3 databases that we need to link
cand_liin <- cand_liin %>%
  mutate(
    status_id = Patient.Identifier,
    patient_id = Unique.Person.ID.for.Recipient,
    death_date = case_when(
      is.na(Death.date.determined.from.SSA.database) == F ~ as.Date(Death.date.determined.from.SSA.database, format =
                                                                      "%m/%d/%y"),
      is.na(OPTN.Death.date.by.unique.person) == F ~ as.Date(OPTN.Death.date.by.unique.person, format =
                                                               "%m/%d/%y")
    ),
    age = as.numeric(Calculated.Candidate.Age.in.Months.at.Listing) / 12,
    race = as.factor(
      case_when(
        Patient.s.Race == "128: Native Hawaiian or Other Pacific Islander" ~ "Pacific Islander",
        Patient.s.Race == "2000: Hispanic/Latino" ~ "Hispanic/Latino",
        Patient.s.Race == "64: Asian" ~ "Asian",
        Patient.s.Race == "16: Black or African American" ~ "Black/African-American",
        Patient.s.Race == "32: American Indian or Alaska Native" ~ "Native American",
        Patient.s.Race == "8: White" ~ "White",
        Patient.s.Race == "Multi-Racial" ~ "Multi-Racial",
        TRUE ~ NA_character_
      )
    ),
    gender = as.factor(
      case_when(
        Patient.s.Gender == "F" ~ "F",
        Patient.s.Gender == "M" ~ "M",
        TRUE ~ NA_character_
      )
    ),
    height = as.numeric(Candidate.s.Height..stored.in.cm.),
    weight = as.numeric(Candidate.s.Weight.in.kilograms),
    accept_incompatible_blood_type = as.factor(
      case_when(
        Accept.an.incompatible.blood.type. == "Y" ~ "Y",
        Accept.an.incompatible.blood.type. == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    accept_a2_donor = as.factor(
      case_when(
        Accept.A2.donor. == "Y" ~ "Y",
        Accept.A2.donor. == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    accept_extra_corporeal_liver = as.factor(
      case_when(
        Accept.an.Extra.corporeal.Liver. == "Y" ~ "Y",
        Accept.an.Extra.corporeal.Liver. == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    accept_liver_segment = as.factor(
      case_when(
        Accept.LI.segment == "Y" ~ "Y",
        Accept.LI.segment == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    accept_HBV_positive_donor = as.factor(
      case_when(
        Accept.an.Hepatitis.B.Core.Antibody.Positive.Donor. == "Y" ~ "Y",
        Accept.an.Hepatitis.B.Core.Antibody.Positive.Donor. == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    accept_HCV_positive_donor = as.factor(
      case_when(
        Accept.an.HCV.Antibody.Positive.Donor. == "Y" ~ "Y",
        Accept.an.HCV.Antibody.Positive.Donor. == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    patient_state = as.factor(
      case_when(
        Patient.s.Permanent.State == "ZZ: UNKNOWN" ~ NA_character_,
        Patient.s.Permanent.State %in% c(
          "AS: PAGO PAGO",
          "GU: GUAM",
          "MP: SAIPAN MARIANA ISLANDS",
          "PR: PUERTO RICO",
          "VI: VIRGIN ISLANDS"
        ) == T ~ "U.S territory",
        Patient.s.Permanent.State == "NA: FOREIGN COUNTRY" ~ "Non-U.S",
        TRUE ~ as.character(Patient.s.Permanent.State)
      )
    ),
    patient_educational_status = as.factor(
      case_when(
        Patient.s.Educational.Status == "1: NONE" ~ "none",
        Patient.s.Educational.Status == "2: GRADE SCHOOL (0-8)" ~ "0-8",
        Patient.s.Educational.Status == "3: HIGH SCHOOL (9-12) or GED" ~ "9-12",
        Patient.s.Educational.Status == "4: ATTENDED COLLEGE/TECHNICAL SCHOOL" ~ "some college / technical school",
        Patient.s.Educational.Status == "5: ASSOCIATE/BACHELOR DEGREE" ~ "associate/bachelor degree",
        Patient.s.Educational.Status == "6: POST-COLLEGE GRADUATE DEGREE" ~ "post-college / graduate degree",
        TRUE ~ NA_character_
      )
    ),
    medical_condition = as.factor(
      case_when(
        Medical.Condition == "1: IN INTENSIVE CARE UNIT" ~ "in ICU",
        Medical.Condition == "2: HOSPITALIZED NOT IN ICU" ~ "hospitalized not in ICU",
        Medical.Condition == "3: NOT HOSPITALIZED" ~ "not hospitalized",
        TRUE ~ NA_character_
      )
    ),
    patient_on_life_support = as.factor(
      case_when(
        Patient.on.Life.Support == "N" ~ "N",
        Patient.on.Life.Support == "Y" ~ "Y",
        TRUE ~ NA_character_
      )
    ),
    functional_status = as.factor(
      case_when(
        Patient.s.Functional.Status == "1: Performs activities of daily living with NO assistance." |
          Patient.s.Functional.Status == "2100: 100% - Normal, no complaints, no evidence of disease" |
          Patient.s.Functional.Status == "2090: 90% - Able to carry on normal activity: minor symptoms of disease" |
          Patient.s.Functional.Status == "2080: 80% - Normal activity with effort: some symptoms of disease" |
          Patient.s.Functional.Status == "2070: 70% - Cares for self: unable to carry on normal activity or active work" ~ "no assistance",
        Patient.s.Functional.Status == "2: Performs activities of daily living with SOME assistance." |
          Patient.s.Functional.Status == "2060: 60% - Requires occasional assistance but is able to care for needs" |
          Patient.s.Functional.Status == "2050: 50% - Requires considerable assistance and frequent medical care" |
          Patient.s.Functional.Status == "2040: 40% - Disabled: requires special care and assistance" |
          Patient.s.Functional.Status == "2030: 30% - Severely disabled: hospitalization is indicated, death not imminent" ~ "some assistance",
        Patient.s.Functional.Status == "3: Performs activities of daily living with TOTAL assistance." |
          Patient.s.Functional.Status == "2020: 20% - Very sick, hospitalization necessary: active treatment necessary" |
          Patient.s.Functional.Status == "2010: 10% - Moribund, fatal processes progressing rapidly" ~ "total assistance",
        TRUE ~ NA_character_
      )
    ),
    primary_diagnosis = as.factor(
      case_when(
        Primary.Diagnosis %in% c(
          "999: OTHER SPECIFY",
          "4593: LI:Hepatitis C: Chronic or Acute",
          "4592: LI:Hepatitis B: Chronic or Acute",
          "4520: LI:TRAUMA OTHER SPECIFY",
          "4510: LI:GRAFT VS. HOST DIS SEC TO NON-LI TX",
          "4500: LI:TPN/HYPERALIMENTATION IND LIVER DISEASE",
          "4455: LI:BENIGN TUMOR: OTHER SPECIFY",
          "4451: LI:BENIGN TUMOR: POLYCYSTIC LIVER DISEASE",
          "4450: LI:BENIGN TUMOR: HEPATIC ADENOMA",
          "4285: LI:CYSTIC FIBROSIS",
          "4290: LI:BUDD-CHIARI SYNDROME"
        ) == T ~ "Other",
        Primary.Diagnosis %in% c(
          "4430: LI:SECONDARY HEPATIC MALIGNANCY OTHER SPECIFY",
          "4420: LI:BILE DUCT CANCER: (CHOLANGIOMA, BILIARY TRACT CARCINOMA)",
          "4410: LI:PLM: OTHER SPECIFY (I.E., KLATZKIN TUMOR, LEIOMYSARCOMA)",
          "4405: LI:PLM: HEMANGIOENDOTHELIOMA, HEMANGIOSARCOMA, ANGIOSARCOMA",
          "4404: LI:PLM: HEPATOBLASTOMA (HBL)",
          "4403: LI:PLM: CHOLANGIOCARCINOMA (CH-CA)",
          "4402: LI:PLM: FIBROLAMELLAR (FL-HC)",
          "4401: LI:PLM: HEPATOMA (HCC) AND CIRRHOSIS",
          "4400: LI:PLM: HEPATOMA - HEPATOCELLULAR CARCINOMA"
        ) == T ~ "Malignant neoplasm",
        Primary.Diagnosis %in% c(
          "4315: LI:METDIS: OTHER SPECIFY",
          "4308: LI:METDIS: MAPLE SYRUP URINE DISEASE",
          "4307: LI:METDIS: PRIMARY OXALOSIS/OXALURIA, HYPEROXALURIA",
          "4306: LI:METDIS: TYROSINEMIA",
          "4305: LI:METDIS: HYPERLIPIDEMIA-II, HOMOZYGOUS HYPERCHOLESTEROLEMIA",
          "4304: LI:METDIS: GLYC STOR DIS TYPE IV (GSD-IV)",
          "4303: LI:METDIS: GLYC STOR DIS TYPE I (GSD-I)",
          "4302: LI:METDIS: HEMOCHROMATOSIS - HEMOSIDEROSIS",
          "4301: LI:METDIS: WILSON'S DISEASE, OTHER COPPER METABOLISM DISORDER",
          "4300: LI:METDIS: ALPHA-1-ANTITRYPSIN DEFIC A-1-A"
        ) == T ~ "Metabolic",
        Primary.Diagnosis %in% c(
          "4275: LI:BILIARY ATRESIA OR HYPOPLASIA: OTHER, SPECIFY",
          "4272: LI:BILIARY HYPOPLASIA: ALAGILLE’S SYNDROME (PAUCITY OF INTRAHEPATIC BILE DUCT)",
          "4271: LI:BILIARY HYPOPLASIA: NONSYNDROMIC PAUCITY OF INTRAHEPATIC BILE DUCT",
          "4270: LI:BILIARY ATRESIA: EXTRAHEPATIC",
          "4265: LI:NEONATAL HEPATITIS OTHER SPECIFY",
          "4264: LI:NEONATAL CHOLESTATIC LIVER DISEASE",
          "4260: LI:CHOLES LIVER DISEASE: OTHER SPECIFY",
          "4255: LI:FAMILIAL CHOLESTASIS: OTHER SPECIFY",
          "4250: LI:FAMILIAL CHOLESTASIS: BYLER'S DISEASE",
          "4245: LI:PSC: OTHER SPECIFY",
          "4242: LI:PSC: NO BOWEL DISEASE",
          "4241: LI:PSC: ULCERATIVE COLITIS",
          "4240: LI:PSC: CROHN'S DISEASE",
          "4235: LI:SEC BILIARY CIRRHOSIS: OTHER SPECIFY",
          "4231: LI:SEC BILIARY CIRRHOSIS: CHOLEDOCHOL CYST",
          "4230: LI:SEC BILIARY CIRRHOSIS: CAROLI'S DISEASE",
          "4220: LI:PRIMARY BILIARY CIRRHOSIS (PBC)"
        ) == T ~ "Cholestatic",
        Primary.Diagnosis %in% c(
          "4217: LI:ACUTE ALCOHOLIC HEPATITIS",
          "4216: LI:ALCOHOLIC CIRRHOSIS WITH HEPATITIS C",
          "4215: LI:ALCOHOLIC CIRRHOSIS",
          "4214: LI:CIRRHOSIS: FATTY LIVER (NASH)",
          "4213: LI:CIRRHOSIS: CRYPTOGENIC (IDIOPATHIC)",
          "4212: LI:CIRRHOSIS: AUTOIMMUNE",
          "4210: LI:CIRRHOSIS: OTHER, SPECIFY (E.G., HISTIOCYTOSIS, SARCOIDOSIS, GRANULOMATOUS)",
          "4209: LI:CIRRHOSIS: CHRONIC ACTIVE HEPATITIS: ETIOLOGY UNKNOWN",
          "4208: LI:CIRRHOSIS: CRYPTOGENIC- IDIOPATHIC",
          "4207: LI:CIRRHOSIS: TYPE B AND D",
          "4206: LI:CIRRHOSIS: TYPE B AND C",
          "4205: LI:CIRRHOSIS: TYPE D",
          "4204: LI:CIRRHOSIS: TYPE C",
          "4203: LI:CIRRHOSIS: TYPE NON A, NON B",
          "4202: LI:CIRRHOSIS: TYPE B- HBSAG+",
          "4201: LI:CIRRHOSIS: TYPE A",
          "4200: LI:CIRRHOSIS: DRUG/INDUST EXPOSURE OTHER SPECIFY",
          "4280: LI:CONGENITAL HEPATIC FIBROSIS"
        ) == T ~ "Non-cholestatic",
        Primary.Diagnosis %in% c(
          "4110: LI:AHN: OTHER, SPECIFY (E.G., ACUTE VIRAL INFECTION, AUTOIMMUNE HEPATITIS - FULMINANT)",
          "4108: LI:AHN: ETIOLOGY UNKNOWN",
          "4107: LI:AHN: TYPE B AND D",
          "4106: LI:AHN: TYPE B AND C",
          "4105: LI:AHN: TYPE D",
          "4104: LI:AHN: TYPE C",
          "4102: LI:AHN: TYPE B- HBSAG+",
          "4101: LI:AHN: TYPE A",
          "4100: LI:AHN: DRUG OTHER SPECIFY"
        ) == T ~ "Fulminant hepatic failure",
        TRUE ~ NA_character_
      )
    ),
    diabetes = as.factor(
      case_when(
        Diabetes %in% c("2: Type I", "3: Type II", "4: Type Other", "5: Type Unknown") == T ~ "Y",
        Diabetes == "1: No" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    history_of_malignancy = as.factor(
      case_when(
        Any.previous.Malignancy == "Y" ~ "Y",
        Any.previous.Malignancy == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    coronary_artery_disease = as.factor(
      case_when(
        Angina.Coronary.Artery.Disease %in% c(
          "30: Stable angina - activity level resulting in angina is unspecified - old code 3",
          "6: Unstable angina",
          "7: Angina, stability unknown"
        ) == T ~ "Y",
        Angina.Coronary.Artery.Disease == "1: No angina" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    hypertension = as.factor(
      case_when(
        Drug.Treated.Systemic.Hypertension == "Y" ~ "Y",
        Drug.Treated.Systemic.Hypertension == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    COPD = as.factor(
      case_when(
        Drug.Treated.COPD == "Y" ~ "Y",
        Drug.Treated.COPD == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    symptomatic_cerebrovascular_disease = as.factor(
      case_when(
        Symptomatic.Cerebrovascular.Disease == "Y" ~ "Y",
        Symptomatic.Cerebrovascular.Disease == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    symptomatic_peripheral_vascular_disease = as.factor(
      case_when(
        Symptomatic.Peripheral.Vascular.Disease == "Y" ~ "Y",
        Symptomatic.Peripheral.Vascular.Disease == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    pulmonary_embolism = as.factor(
      case_when(
        Pulmonary.Embolism == "Y" ~ "Y",
        Pulmonary.Embolism == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    spontaneous_bacterial_peritonitis = as.factor(
      case_when(
        Spontaneous.Bacterial.Peritonitis == "Y" ~ "Y",
        Spontaneous.Bacterial.Peritonitis == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    history_of_PV_thrombosis = as.factor(
      case_when(
        History.of.Portal.Vein.Thrombosis == "Y" ~ "Y",
        History.of.Portal.Vein.Thrombosis == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    history_of_TIPSS = as.factor(
      case_when(
        History.of.TIPSS == "Y" ~ "Y",
        History.of.TIPSS == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    variceal_bleeding = as.factor(
      case_when(
        Variceal.Bleeding.within.Last.Two.Weeks == "Y" ~ "Y",
        Variceal.Bleeding.within.Last.Two.Weeks == "N" ~ "N",
        TRUE ~ NA_character_
      )
    )
  ) %>% mutate(death = as.numeric(case_when(is.na(death_date) == F  ~ 1,
                                            TRUE ~ 0))) %>%
  filter(age >= 18) %>%
  filter(Organ.of.this.waitlisted.record == "LI: Liver") %>%
  filter(Prev.Liver.Tx == 0)
  
stathist_liin <- stathist_liin %>%
  mutate(
    status_id = Patient.Identifier...from.WL.for.A...R.Cands.TCR.for.L.Cands,
    MELDcat = as.factor(
      case_when(
        as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) >= 35 ~ ">= 35",
        as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) >= 30 &
          as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) < 35 ~ "30-35",
        as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) >= 25 &
          as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) < 30 ~ "25-29",
        as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) >= 20 &
          as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) < 25 ~ "20-24",
        as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) >= 15 &
          as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) < 20 ~ "15-19",
        as.numeric(str_sub(SRTR.MELD.PELD.given, -2)) < 15 ~ "< 15",
        TRUE ~ NA_character_
      )
    ),
    MELD = as.numeric(str_sub(SRTR.MELD.PELD.given, -2)),
    MELD_exception = as.factor(
      case_when(
        Exception.MELD.PELD == 1 ~ "Y",
        Exception.MELD.PELD == 0 ~ "N",
        TRUE ~ NA_character_
      )
    ), 
    status1 = as.factor(
      case_when(
        WL.Status.During.Given.Period %in% c(
          "3010: Status 1",
          "6010: Status 1",
          "6011: Status 1A",
          "6012: Status 1B"
        ) == T ~ "status1",
        TRUE ~ "other status"
      )
    ),
    INR = as.numeric(
      International.Normalized.Ratio..how.fast.the.patients.blood.clots.relative.to.the.control..used.for.MELD.
    ),
    albumin = as.numeric(albumin..used.for.MELD.),
    ascites = as.factor(
      case_when(
        ascites..used.for.MELD. == "1: Absent" ~ "absent",
        ascites..used.for.MELD. == "2: Slight" ~ "slight",
        ascites..used.for.MELD. == "3: Moderate" ~ "moderate",
        TRUE ~ NA_character_
      )
    ),
    bilirubin = as.numeric(bilirubin..used.for.MELD.),
    creatinine = as.numeric(serum.creatinine..used.for.MELD.),
    dialysis_past_week = as.factor(
      case_when(
        Last.non.blank.val..of.dialysis.within.prior.week == "Y" ~ "Y",
        Last.non.blank.val..of.dialysis.within.prior.week == "N" ~ "N",
        TRUE ~ NA_character_
      )
    ),
    encephalopathy = as.factor(
      case_when(
        encephalopathy..used.for.MELD. == "1: None" ~ "none",
        encephalopathy..used.for.MELD. == "2: 1-2" ~ "1-2",
        encephalopathy..used.for.MELD. == "3: 3-4" ~ "3-4",
        TRUE ~ NA_character_
      )
    ),
    ascites = as.factor(
      case_when(
        ascites..used.for.MELD. == "1: Absent" ~ "absent",
        ascites..used.for.MELD. == "2: Slight" ~ "absent",
        ascites..used.for.MELD. == "3: Moderate" ~ "absent",
        TRUE ~ NA_character_
      )
    ),
    sodium = as.numeric(serum.sodium..used.for.MELD.),
    height = as.numeric(Candidate.s.Height..stored.in.cm.),
    weight = as.numeric(Candidate.s.Weight.in.kilograms),
    record_start = as.Date(WL.Status.Period.Begin.Date.Time, format = "%d%b%y"),
    record_end = as.Date(WL.Status.Period.End.Date.Time, format = "%d%b%y"),
    listing_date = as.Date(
      Listing.Date...date.time.candidate.was.physically.added.to.the.waiting.list...this.date.time.does.not.change.for.transfers,
      format = "%m/%d/%y"
    )
  ) %>%
  filter(Organ.of.this.waitlisted.record == "LI: Liver") %>%
  filter(status_id %in% cand_liin$status_id)

tx_li <-
  join(
    tx_li %>% filter(Prev.Liver.Tx == 0) %>% dplyr::select(-Anti.HCV),
    donor_deceased %>% dplyr::select(
      c(
        Encrypted.Unique.Donor.ID..all.donors....foreign.key,
        Anti.HIV.I.II.,
        HBsAg..Hepatitis.B.Surface.Antigen.,
        Anti.HCV
      )
    ),
    by = "Encrypted.Unique.Donor.ID..all.donors....foreign.key"
  ) %>%
  mutate(
    patient_id = Unique.person.ID.to.replace.SSN,
    transplant_date = as.Date(Transplant.Date, format = "%m/%d/%y"),
    last_graft_follow_up_date = as.Date(Last.Graft.Follow.up.Date, format =
                                          "%m/%d/%y"),
    non_heart_beating_donor = as.factor(
      case_when(
        Non.Heart.Beating.Donor == "N" ~ "N",
        Non.Heart.Beating.Donor == "Y" ~ "Y",
        TRUE ~ NA_character_
      )
    ),
    deceased_donor = as.factor(
      case_when(
        Donor.Type..C.deceased..L.living. == "C" ~ "deceased",
        Donor.Type..C.deceased..L.living. == "L" ~ "living",
        TRUE ~ NA_character_
      )
    ),
    CDC_increased_risk =
      case_when(
        Does.the.Donor.meet.CDC.guidelines.for.High.Risk.for.an.organ.donor. == "Y" ~ 1,
        Does.the.Donor.meet.CDC.guidelines.for.High.Risk.for.an.organ.donor. == "N" ~ 0,
        TRUE ~ NA_real_
      ), 
    HIV_status_of_donor = as.factor(
      case_when(
        Anti.HIV.I.II. == "N: Negative" ~ "negative",
        Anti.HIV.I.II. %in% c(
          "C: Cannot Disclose",
          "I: Indeterminate",
          "ND: Not Done",
          "PD: Pending",
          "U: Unknown"
        ) ~ "positive or indeterminate",
        TRUE ~ NA_character_
      )
    ),
    HBV_status_of_donor = as.factor(
      case_when(
        HBsAg..Hepatitis.B.Surface.Antigen. == "N: Negative" ~ "negative",
        HBsAg..Hepatitis.B.Surface.Antigen. %in% c(
          "C: Cannot Disclose",
          "I: Indeterminate",
          "ND: Not Done",
          "PD: Pending",
          "U: Unknown"
        ) ~ "positive or indeterminate",
        TRUE ~ NA_character_
      )
    ),
    HCV_status_of_donor = as.factor(
      case_when(
        Anti.HCV == "N: Negative" ~ "negative",
        Anti.HCV %in% c(
          "C: Cannot Disclose",
          "I: Indeterminate",
          "ND: Not Done",
          "PD: Pending",
          "U: Unknown"
        ) ~ "positive or indeterminate",
        TRUE ~ NA_character_
      )
    )
  ) %>%
  filter(patient_id %in% cand_liin$patient_id)

#join the 3 databases into a combined database
candidates <- join(stathist_liin %>% dplyr::select(
  c(
    MELD,
    MELDcat,
    MELD_exception,
    status1,
    INR,
    albumin,
    ascites,
    bilirubin,
    creatinine,
    dialysis_past_week,
    encephalopathy,
    ascites,
    sodium,
    record_start,
    record_end,
    listing_date,
    status_id
  )
),
cand_liin %>% dplyr::select(
  c(
    age,
    race,
    gender,
    height,
    weight,
    accept_incompatible_blood_type,
    accept_a2_donor,
    accept_extra_corporeal_liver,
    accept_liver_segment,
    accept_HBV_positive_donor,
    accept_HCV_positive_donor,
    patient_state,
    patient_educational_status,
    medical_condition,
    patient_on_life_support,
    functional_status,
    primary_diagnosis,
    diabetes,
    history_of_malignancy,
    coronary_artery_disease,
    hypertension,
    COPD,
    symptomatic_cerebrovascular_disease,
    symptomatic_peripheral_vascular_disease,
    pulmonary_embolism,
    spontaneous_bacterial_peritonitis,
    variceal_bleeding,
    history_of_PV_thrombosis,
    history_of_TIPSS,
    Reason.why.candidate.was.removed..removal.code.,
    death,
    death_date,
    status_id,
    patient_id
  )
),
by = "status_id") %>% join(., tx_li %>% dplyr::select(
  c(
    non_heart_beating_donor,
    deceased_donor,
    CDC_increased_risk,
    HIV_status_of_donor,
    HBV_status_of_donor,
    HCV_status_of_donor,
    patient_id,
    transplant_date,
    last_graft_follow_up_date
  )
), by = "patient_id") %>% group_by(patient_id) %>%
  mutate(listing_date = min(listing_date)) %>% 
  filter(year(listing_date) >= 2005 &
           year(listing_date) <= 2015) %>%
  filter(record_start <= as.Date("05/31/16", format = "%m/%d/%y")) %>%
  mutate(record_end = case_when(
    is.na(record_end) == T ~ as.Date("05/31/16", format = "%m/%d/%y"),
    TRUE ~ pmin(record_end, as.Date("05/31/16", format = "%m/%d/%y"))
  )) %>% 
  mutate(
    age = min(age),
    max_record_end = max(record_end),
    reason_record_end = Reason.why.candidate.was.removed..removal.code.[which(record_end == max(record_end))[1]],
    transplanted = as.numeric(case_when(is.na(transplant_date) == F ~ 1, TRUE ~ 0)),
    censored_at_transplant = case_when((HIV_status_of_donor == "negative" &
                                        HCV_status_of_donor == "negative" &
                                        HBV_status_of_donor == "negative" &
                                        non_heart_beating_donor == "N" &
                                        deceased_donor == "deceased" &
                                        is.na(CDC_increased_risk) == F) ~ 0,
                                       is.na(transplant_date) == F ~ 1)
  ) %>%
  mutate(
    last_follow_up = case_when(
      death == 1 ~ min(death_date, as.Date("05/31/16", format = "%m/%d/%y")),
      transplanted == 1 &
        death == 0 ~ min(last_graft_follow_up_date, as.Date("05/31/16", format = "%m/%d/%y")),
      transplanted == 0 &
        death == 0 ~ max_record_end
    )
  ) %>%
  mutate(
    censored = case_when(death != 1 & last_follow_up < as.Date("05/31/16", format = "%m/%d/%y") ~ 1,
                         TRUE ~ 0)
  ) %>% 
  {
    .[order(.$patient_id, .$record_start), ]
  } %>% ungroup() %>%
  mutate(year_of_listing = as.factor(year(listing_date)),
         year_of_transplant = as.factor(year(transplant_date)))

#remove duplicated records
candidates <- bind_rows(
  candidates[candidates$patient_id %in% unique((candidates %>% group_by(patient_id) %>% filter(duplicated(record_start) ==
                                                                                                 T))$patient_id),] %>%
    group_by(patient_id, record_start) %>% filter((
      is.na(MELD) + is.na(status1) + is.na(MELD_exception)
    )  ==
      min((
        is.na(MELD) + is.na(status1)  + is.na(MELD_exception)
      ))) %>%
    filter(duplicated(record_start) == F),
  candidates[!candidates$patient_id %in% unique((candidates %>% group_by(patient_id) %>% filter(duplicated(record_start) ==
                                                                                                  T))$patient_id), ]
) %>% filter(record_start <= transplant_date |
               is.na(transplant_date) == T) %>%
               {
                 .[order(.$patient_id, .$record_start), ]
               } %>% ungroup()

#carry forward covariate information
candidates <-
  candidates %>% group_by(patient_id) %>% fill() %>% ungroup()

#save candidates file
save(candidates, file="candidates.RData")

#create expanded file
candidates_expanded <-
  bind_rows(pblapply(unique(candidates$patient_id), function(x) {
    temp <- candidates %>% filter(patient_id == x)
    temp %>% mutate(next_date = lead(record_start, 1)) %>%
      mutate(next_date = case_when(is.na(next_date) == T ~ last_follow_up,
                                   TRUE ~ next_date)) %>%
      mutate(days = map2(record_start, next_date, `:`)) %>%
      unnest() %>%  group_by(days) %>% filter(row_number() == n()) %>% ungroup() %>%
      mutate(days_since_start = as.numeric(seq_along(1:(n()))) - 1) %>% filter(
        days_since_start == 0 |
          days_since_start == max(days_since_start) |
          days_since_start %% 30 == 0
      ) %>% mutate(date = min(record_start) + days_since_start)
  }))

#save expanded candidates file
save(candidates_expanded, file="candidates_expanded.RData")

#place censoring and death variables
candidates_analysis <-
  candidates_expanded %>% group_by(patient_id) %>%  mutate(
    death_event = case_when(
      death_date <= last_follow_up &
        (
          (death_date == lead(date) & lead(date) == max(date) & (lead(date)-date != 30)) |
            (death_date == date & date == max(date) & (date-lag(date) == 30)) |
            (death_date >= date &
               death_date < lead(date)) |
            (death_date < min(date) &
               date == min(date)) |
            (n() == 1 & death == 1)
        ) ~ 1,
      TRUE ~ 0
    ),
    censoring_event = case_when(censored == 1 &
                                  ((last_follow_up == lead(date) & lead(date) == max(date) & (lead(date)-date != 30)) |
                                  (last_follow_up == date & date == max(date) & (date-lag(date) == 30))) ~ 1,
                                TRUE ~ 0),
    transplant_event = case_when(
      transplant_date <= last_follow_up &
        (
          (transplant_date == lead(date) & lead(date) == max(date) & (lead(date)-date != 30)) |
            (transplant_date == date & date == max(date) & (date-lag(date) == 30)) |
            (transplant_date >= date &
               transplant_date < lead(date)) |
            transplant_date < min(date) &
            date == min(date) | (n() == 1 & transplanted == 1)
        ) ~ 1,
      TRUE ~ 0
    ),
    post_transplant = case_when(transplant_date < date ~ 1,
                                TRUE ~ 0),
    time_since_transplant = as.numeric(case_when(
      is.na(transplant_date) == F ~ pmax(date - transplant_date, 0),
      TRUE ~ 0
    ))
  ) %>% ungroup() %>% mutate(
    transplant_and_increased_risk = case_when(transplant_event == 1 & censored_at_transplant == 0 & CDC_increased_risk == 1 ~ 1,
                                         TRUE ~ 0),
    transplant_and_standard_risk = case_when(transplant_event == 1 & censored_at_transplant == 0 & CDC_increased_risk == 0 ~ 1,
                                         TRUE ~ 0),
    transplant_and_censored_at_tx = case_when(transplant_event == 1 & censored_at_transplant == 1 ~ 1,
                                         TRUE ~ 0))

#remove rows after death/censoring]
candidates_analysis <-
  candidates_analysis %>% group_by(patient_id) %>%
  mutate(
    filtervardeath = case_when(death_event == 1 ~ 0, TRUE ~ cumsum(death_event)),
    filtervarcensor = case_when(censoring_event == 1 ~ 0, TRUE ~ cumsum(censoring_event))
  ) %>%
  filter(filtervardeath == 0 &
           filtervarcensor == 0) %>% ungroup() %>% select(-c(filtervardeath, filtervarcensor))

#filter person-visits which are administratively censored
candidates_analysis <-
  candidates_analysis %>% 
  filter((date + 30) <= as.Date("05/31/16", format = "%m/%d/%y")) 

#select variables for analysis file
candidates_analysis <-
  candidates_analysis %>% dplyr::select(
    c(
      patient_id,
      status_id,
      year_of_listing,
      date,
      days_since_start,
      last_follow_up,
      reason_record_end,
      censored,
      censoring_event,
      death,
      death_event,
      death_date,
      transplanted,
      censored_at_transplant,
      transplant_event,
      transplant_and_increased_risk,
      transplant_and_standard_risk,
      transplant_and_censored_at_tx,
      transplant_date,
      post_transplant,
      time_since_transplant,
      CDC_increased_risk,
      year_of_transplant,
      MELD,
      MELD_exception,
      status1,
      gender,
      age,
      race,
      height,
      weight,
      accept_incompatible_blood_type,
      accept_extra_corporeal_liver,
      accept_liver_segment,
      accept_HBV_positive_donor,
      accept_HCV_positive_donor,
      patient_on_life_support,
      functional_status,
      primary_diagnosis,
      spontaneous_bacterial_peritonitis,
      history_of_PV_thrombosis,
      history_of_TIPSS
    )
  )

ids_with_missing_baseline <- 
candidates_analysis %>% 
  filter(days_since_start==0) %>% 
  {.[which(
    is.na(.$MELD) | 
      is.na(.$MELD_exception) |
      is.na(.$status1) |
      is.na(.$gender) |
      is.na(.$age) |
      is.na(.$race) |
      is.na(.$height) |
      is.na(.$weight) |
      is.na(.$accept_incompatible_blood_type) |
      is.na(.$accept_extra_corporeal_liver) |
      is.na(.$accept_liver_segment) |
      is.na(.$accept_HBV_positive_donor) |
      is.na(.$accept_HCV_positive_donor) |
      is.na(.$patient_on_life_support) |
      is.na(.$functional_status) |
      is.na(.$primary_diagnosis) |
      is.na(.$spontaneous_bacterial_peritonitis) |
      is.na(.$history_of_PV_thrombosis) |
      is.na(.$history_of_TIPSS)
             ),]$patient_id}

candidates_analysis <- 
  candidates_analysis %>% filter(!patient_id %in% ids_with_missing_baseline)

#fix non-time-varying covariates at baseline level
candidates_analysis <-
  candidates_analysis %>% group_by(patient_id) %>%
  mutate(
    baseline_MELD = first(MELD),
    baseline_MELD_exception = first(MELD_exception),
    status1 = first(status1),
    gender = first(gender),
    age = first(age),
    race = first(race),
    height = first(height),
    weight = first(weight),
    accept_incompatible_blood_type = first(accept_incompatible_blood_type),
    accept_extra_corporeal_liver = first(accept_extra_corporeal_liver),
    accept_liver_segment = first(accept_liver_segment),
    accept_HBV_positive_donor = first(accept_HBV_positive_donor),
    accept_HCV_positive_donor = first(accept_HCV_positive_donor),
    patient_on_life_support = first(patient_on_life_support),
    functional_status = first(functional_status),
    primary_diagnosis = first(primary_diagnosis),
    spontaneous_bacterial_peritonitis = first(spontaneous_bacterial_peritonitis),
    history_of_PV_thrombosis = first(history_of_PV_thrombosis),
    history_of_TIPSS = first(history_of_TIPSS)
  ) %>% ungroup()

#free up memory after cleaning
rm(stathist_liin, cand_liin, tx_li, donor_deceased, candidates, candidates_expanded, ids_with_missing_baseline)
gc()

#save analysis file
save(candidates_analysis, file="candidates_analysis.RData")