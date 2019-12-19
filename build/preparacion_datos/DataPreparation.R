
# Buscamos scripts de funciones utiles y cargamos los datos
source("../comprension_datos/Utils.R")
source("../comprension_datos/00-load.R")

#Convertimos los nombres de las variables a minúsculas
colnames(walmart_train) <- tolower(colnames(walmart_train) )
colnames(walmart_train)

#Cuenta por departamento
walmart_train %>% group_by(triptype) %>% 
  mutate(
    num_count = length(unique(departmentdescription))
  ) %>% distinct(triptype, num_count) %>% ungroup()


#Repaso de los datos
walmart_data <- walmart_train
glimpse(walmart_data)
describe(walmart_data)
data_integrity(walmart_data)

## Selección de información
format_data <- function(data) {
  colnames(data) <- tolower(colnames(data) )
  data
}

fill_na <- function(data) {
  data %>% mutate(
    prod_miss = ifelse(is.na(upc), 1, 0),
    upc = ifelse(is.na(upc), 0, upc),
    finelinenumber = ifelse(is.na(finelinenumber), 0, finelinenumber)
  )
}


select_features <- function(data) {
  data %>% dplyr::select(-weekday, -upc, -visitnumber )
}


## Ingeniería de características
create_features <- function(data) {
  data %>% mutate(
    weekend = case_when(
      weekday == "Saturday" ~ 1,
      weekday == "Sunday" ~ 1,
      TRUE ~ 0
    ),
    day = case_when(
      weekday == "Saturday" ~ 6,
      weekday == "Sunday" ~ 7,
      weekday == "Monday" ~ 1,
      weekday == "Tuesday" ~ 2,
      weekday == "Wednesday" ~ 3,
      weekday == "Thursday" ~ 4,
      weekday == "Friday" ~ 5,
      TRUE ~ 0
    ),
    dep_id = case_when(
      departmentdescription == 'FINANCIAL SERVICES' ~ 1,
      departmentdescription == 'SHOES' ~ 2,
      departmentdescription == 'PERSONAL CARE' ~ 3,
      departmentdescription == 'PAINT AND ACCESSORIES' ~ 4,
      departmentdescription == 'DSD GROCERY' ~ 5,
      departmentdescription == 'MEAT - FRESH & FROZEN' ~ 6,
      departmentdescription == 'DAIRY' ~ 7,
      departmentdescription == 'PETS AND SUPPLIES' ~ 8,
      departmentdescription == 'HOUSEHOLD CHEMICALS/SUPP' ~ 9,
      departmentdescription == 'NULL' ~ 10,
      departmentdescription == 'IMPULSE MERCHANDISE' ~ 11,
      departmentdescription == 'PRODUCE' ~ 12,
      departmentdescription == 'CANDY, TOBACCO, COOKIES' ~ 13,
      departmentdescription == 'GROCERY DRY GOODS' ~ 14,
      departmentdescription == 'BOYS WEAR' ~ 15,
      departmentdescription == 'FABRICS AND CRAFTS' ~ 16,
      departmentdescription == 'JEWELRY AND SUNGLASSES' ~ 17,
      departmentdescription == 'MENS WEAR' ~ 18,
      departmentdescription == 'ACCESSORIES' ~ 19,
      departmentdescription == 'HOME MANAGEMENT' ~ 20,
      departmentdescription == 'FROZEN FOODS' ~ 21,
      departmentdescription == 'SERVICE DELI' ~ 22,
      departmentdescription == 'INFANT CONSUMABLE HARDLINES' ~ 23,
      departmentdescription == 'PRE PACKED DELI' ~ 24,
      departmentdescription == 'COOK AND DINE' ~ 25,
      departmentdescription == 'PHARMACY OTC' ~ 26,
      departmentdescription == 'LADIESWEAR' ~ 27,
      departmentdescription == 'COMM BREAD' ~ 28,
      departmentdescription == 'BAKERY' ~ 29,
      departmentdescription == 'HOUSEHOLD PAPER GOODS' ~ 30,
      departmentdescription == 'CELEBRATION' ~ 31,
      departmentdescription == 'HARDWARE' ~ 32,
      departmentdescription == 'BEAUTY' ~ 33,
      departmentdescription == 'AUTOMOTIVE' ~ 34,
      departmentdescription == 'BOOKS AND MAGAZINES' ~ 35,
      departmentdescription == 'SEAFOOD' ~ 36,
      departmentdescription == 'OFFICE SUPPLIES' ~ 37,
      departmentdescription == 'LAWN AND GARDEN' ~ 38,
      departmentdescription == 'SHEER HOSIERY' ~ 39,
      departmentdescription == 'WIRELESS' ~ 40,
      departmentdescription == 'BEDDING' ~ 41,
      departmentdescription == 'BATH AND SHOWER' ~ 42,
      departmentdescription == 'HORTICULTURE AND ACCESS' ~ 43,
      departmentdescription == 'HOME DECOR' ~ 44,
      departmentdescription == 'TOYS' ~ 45,
      departmentdescription == 'INFANT APPAREL' ~ 46,
      departmentdescription == 'LADIES SOCKS' ~ 47,
      departmentdescription == 'PLUS AND MATERNITY' ~ 48,
      departmentdescription == 'ELECTRONICS' ~ 49,
      departmentdescription == 'GIRLS WEAR, 4-6X  AND 7-14' ~ 50,
      departmentdescription == 'BRAS & SHAPEWEAR' ~ 51,
      departmentdescription == 'LIQUOR,WINE,BEER' ~ 52,
      departmentdescription == 'SLEEPWEAR/FOUNDATIONS' ~ 53,
      departmentdescription == 'CAMERAS AND SUPPLIES' ~ 54,
      departmentdescription == 'SPORTING GOODS' ~ 55,
      departmentdescription == 'PLAYERS AND ELECTRONICS' ~ 56,
      departmentdescription == 'PHARMACY RX' ~ 57,
      departmentdescription == 'MENSWEAR' ~ 58,
      departmentdescription == 'OPTICAL - FRAMES' ~ 59,
      departmentdescription == 'SWIMWEAR/OUTERWEAR' ~ 60,
      departmentdescription == 'OTHER DEPARTMENTS' ~ 61,
      departmentdescription == 'MEDIA AND GAMING' ~ 62,
      departmentdescription == 'FURNITURE' ~ 63,
      departmentdescription == 'OPTICAL - LENSES' ~ 64,
      departmentdescription == 'SEASONAL' ~ 65,
      departmentdescription == 'LARGE HOUSEHOLD GOODS' ~ 66,
      departmentdescription == '1-HR PHOTO' ~ 67,
      departmentdescription == 'CONCEPT STORES' ~ 68,
      departmentdescription == 'HEALTH AND BEAUTY AIDS' ~ 69
    )
  ) %>% group_by(visitnumber) %>% 
    mutate(
      obj_abs = sum(abs(scancount)),
      num_obj =  sum(scancount),
      porc_devol = 1 - (num_obj/obj_abs),
      devol = ifelse(porc_devol<1,1,0),
      unique_prod = length(unique(finelinenumber))
    ) %>% ungroup()
}


sum_features <- function(data) {
  data %>%
    mutate(
      dep_1= ifelse(dep_id==1,1,0) ,
      dep_2= ifelse(dep_id==2,1,0),
      dep_3= ifelse(dep_id==3,1,0),
      dep_4= ifelse(dep_id==4,1,0),
      dep_5= ifelse(dep_id==5,1,0),
      dep_6= ifelse(dep_id==6,1,0),
      dep_7= ifelse(dep_id==7,1,0),
      dep_8= ifelse(dep_id==8,1,0),
      dep_9= ifelse(dep_id==9,1,0),
      dep_10= ifelse(dep_id==10,1,0),
      dep_11= ifelse(dep_id==11,1,0),
      dep_12= ifelse(dep_id==12,1,0),
      dep_13= ifelse(dep_id==13,1,0),
      dep_14= ifelse(dep_id==14,1,0),
      dep_15= ifelse(dep_id==15,1,0),
      dep_16= ifelse(dep_id==16,1,0),
      dep_17= ifelse(dep_id==17,1,0),
      dep_18= ifelse(dep_id==18,1,0),
      dep_19= ifelse(dep_id==19,1,0),
      dep_20= ifelse(dep_id==20,1,0),
      dep_21= ifelse(dep_id==21,1,0),
      dep_22= ifelse(dep_id==22,1,0),
      dep_23= ifelse(dep_id==23,1,0),
      dep_24= ifelse(dep_id==24,1,0),
      dep_25= ifelse(dep_id==25,1,0),
      dep_26= ifelse(dep_id==26,1,0),
      dep_27= ifelse(dep_id==27,1,0),
      dep_28= ifelse(dep_id==28,1,0),
      dep_29= ifelse(dep_id==29,1,0),
      dep_30= ifelse(dep_id==30,1,0),
      dep_31= ifelse(dep_id==31,1,0),
      dep_32= ifelse(dep_id==32,1,0),
      dep_33= ifelse(dep_id==33,1,0),
      dep_34= ifelse(dep_id==34,1,0),
      dep_35= ifelse(dep_id==35,1,0),
      dep_36= ifelse(dep_id==36,1,0),
      dep_37= ifelse(dep_id==37,1,0),
      dep_38= ifelse(dep_id==38,1,0),
      dep_39= ifelse(dep_id==39,1,0),
      dep_40= ifelse(dep_id==40,1,0),
      dep_41= ifelse(dep_id==41,1,0),
      dep_42= ifelse(dep_id==42,1,0),
      dep_43= ifelse(dep_id==43,1,0),
      dep_44= ifelse(dep_id==44,1,0),
      dep_45= ifelse(dep_id==45,1,0),
      dep_46= ifelse(dep_id==46,1,0),
      dep_47= ifelse(dep_id==47,1,0),
      dep_48= ifelse(dep_id==48,1,0),
      dep_49= ifelse(dep_id==49,1,0),
      dep_50= ifelse(dep_id==50,1,0),
      dep_51= ifelse(dep_id==51,1,0),
      dep_52= ifelse(dep_id==52,1,0),
      dep_53= ifelse(dep_id==53,1,0),
      dep_54= ifelse(dep_id==54,1,0),
      dep_55= ifelse(dep_id==55,1,0),
      dep_56= ifelse(dep_id==56,1,0),
      dep_57= ifelse(dep_id==57,1,0),
      dep_58= ifelse(dep_id==58,1,0),
      dep_59= ifelse(dep_id==59,1,0),
      dep_60= ifelse(dep_id==60,1,0),
      dep_61= ifelse(dep_id==61,1,0),
      dep_62= ifelse(dep_id==62,1,0),
      dep_63= ifelse(dep_id==63,1,0),
      dep_64= ifelse(dep_id==64,1,0),
      dep_65= ifelse(dep_id==65,1,0),
      dep_66= ifelse(dep_id==66,1,0),
      dep_67= ifelse(dep_id==67,1,0),
      dep_68= ifelse(dep_id==68,1,0),
      dep_69= ifelse(dep_id==69,1,0)
    ) %>% 
    group_by(visitnumber) %>% 
    dplyr::summarise(
      prob_1= sum(dep_1)/n(),
      prob_2= sum(dep_2)/n(),
      prob_3= sum(dep_3)/n(),
      prob_4= sum(dep_4)/n(),
      prob_5= sum(dep_5)/n(),
      prob_6= sum(dep_6)/n(),
      prob_7= sum(dep_7)/n(),
      prob_8= sum(dep_8)/n(),
      prob_9= sum(dep_9)/n(),
      prob_10= sum(dep_10)/n(),
      prob_11= sum(dep_11)/n(),
      prob_12= sum(dep_12)/n(),
      prob_13= sum(dep_13)/n(),
      prob_14= sum(dep_14)/n(),
      prob_15= sum(dep_15)/n(),
      prob_16= sum(dep_16)/n(),
      prob_17= sum(dep_17)/n(),
      prob_18= sum(dep_18)/n(),
      prob_19= sum(dep_19)/n(),
      prob_20= sum(dep_20)/n(),
      prob_21= sum(dep_21)/n(),
      prob_22= sum(dep_22)/n(),
      prob_23= sum(dep_23)/n(),
      prob_24= sum(dep_24)/n(),
      prob_25= sum(dep_25)/n(),
      prob_26= sum(dep_26)/n(),
      prob_27= sum(dep_27)/n(),
      prob_28= sum(dep_28)/n(),
      prob_29= sum(dep_29)/n(),
      prob_30= sum(dep_30)/n(),
      prob_31= sum(dep_31)/n(),
      prob_32= sum(dep_32)/n(),
      prob_33= sum(dep_33)/n(),
      prob_34= sum(dep_34)/n(),
      prob_35= sum(dep_35)/n(),
      prob_36= sum(dep_36)/n(),
      prob_37= sum(dep_37)/n(),
      prob_38= sum(dep_38)/n(),
      prob_39= sum(dep_39)/n(),
      prob_40= sum(dep_40)/n(),
      prob_41= sum(dep_41)/n(),
      prob_42= sum(dep_42)/n(),
      prob_43= sum(dep_43)/n(),
      prob_44= sum(dep_44)/n(),
      prob_45= sum(dep_45)/n(),
      prob_46= sum(dep_46)/n(),
      prob_47= sum(dep_47)/n(),
      prob_48= sum(dep_48)/n(),
      prob_49= sum(dep_49)/n(),
      prob_50= sum(dep_50)/n(),
      prob_51= sum(dep_51)/n(),
      prob_52= sum(dep_52)/n(),
      prob_53= sum(dep_53)/n(),
      prob_54= sum(dep_54)/n(),
      prob_55= sum(dep_55)/n(),
      prob_56= sum(dep_56)/n(),
      prob_57= sum(dep_57)/n(),
      prob_58= sum(dep_58)/n(),
      prob_59= sum(dep_59)/n(),
      prob_60= sum(dep_60)/n(),
      prob_61= sum(dep_61)/n(),
      prob_62= sum(dep_62)/n(),
      prob_63= sum(dep_63)/n(),
      prob_64= sum(dep_64)/n(),
      prob_65= sum(dep_65)/n(),
      prob_66= sum(dep_66)/n(),
      prob_67= sum(dep_67)/n(),
      prob_68= sum(dep_68)/n(),
      prob_69= sum(dep_69)/n(),
      obj_abs = sum(abs(scancount)),
      num_obj =  sum(scancount),
      porc_devol = 1 - (num_obj/obj_abs),
      devol = ifelse(porc_devol<1,1,0),
      unique_prod = length(unique(finelinenumber)),
      triptype = mean(triptype),
      day = mean(day),
      weekend = mean(weekend)
    ) %>% ungroup()
}


sum_features_test <- function(data) {
  data %>%
    mutate(
      dep_1= ifelse(dep_id==1,1,0),
      dep_2= ifelse(dep_id==2,1,0),
      dep_3= ifelse(dep_id==3,1,0),
      dep_4= ifelse(dep_id==4,1,0),
      dep_5= ifelse(dep_id==5,1,0),
      dep_6= ifelse(dep_id==6,1,0),
      dep_7= ifelse(dep_id==7,1,0),
      dep_8= ifelse(dep_id==8,1,0),
      dep_9= ifelse(dep_id==9,1,0),
      dep_10= ifelse(dep_id==10,1,0),
      dep_11= ifelse(dep_id==11,1,0),
      dep_12= ifelse(dep_id==12,1,0),
      dep_13= ifelse(dep_id==13,1,0),
      dep_14= ifelse(dep_id==14,1,0),
      dep_15= ifelse(dep_id==15,1,0),
      dep_16= ifelse(dep_id==16,1,0),
      dep_17= ifelse(dep_id==17,1,0),
      dep_18= ifelse(dep_id==18,1,0),
      dep_19= ifelse(dep_id==19,1,0),
      dep_20= ifelse(dep_id==20,1,0),
      dep_21= ifelse(dep_id==21,1,0),
      dep_22= ifelse(dep_id==22,1,0),
      dep_23= ifelse(dep_id==23,1,0),
      dep_24= ifelse(dep_id==24,1,0),
      dep_25= ifelse(dep_id==25,1,0),
      dep_26= ifelse(dep_id==26,1,0),
      dep_27= ifelse(dep_id==27,1,0),
      dep_28= ifelse(dep_id==28,1,0),
      dep_29= ifelse(dep_id==29,1,0),
      dep_30= ifelse(dep_id==30,1,0),
      dep_31= ifelse(dep_id==31,1,0),
      dep_32= ifelse(dep_id==32,1,0),
      dep_33= ifelse(dep_id==33,1,0),
      dep_34= ifelse(dep_id==34,1,0),
      dep_35= ifelse(dep_id==35,1,0),
      dep_36= ifelse(dep_id==36,1,0),
      dep_37= ifelse(dep_id==37,1,0),
      dep_38= ifelse(dep_id==38,1,0),
      dep_39= ifelse(dep_id==39,1,0),
      dep_40= ifelse(dep_id==40,1,0),
      dep_41= ifelse(dep_id==41,1,0),
      dep_42= ifelse(dep_id==42,1,0),
      dep_43= ifelse(dep_id==43,1,0),
      dep_44= ifelse(dep_id==44,1,0),
      dep_45= ifelse(dep_id==45,1,0),
      dep_46= ifelse(dep_id==46,1,0),
      dep_47= ifelse(dep_id==47,1,0),
      dep_48= ifelse(dep_id==48,1,0),
      dep_49= ifelse(dep_id==49,1,0),
      dep_50= ifelse(dep_id==50,1,0),
      dep_51= ifelse(dep_id==51,1,0),
      dep_52= ifelse(dep_id==52,1,0),
      dep_53= ifelse(dep_id==53,1,0),
      dep_54= ifelse(dep_id==54,1,0),
      dep_55= ifelse(dep_id==55,1,0),
      dep_56= ifelse(dep_id==56,1,0),
      dep_57= ifelse(dep_id==57,1,0),
      dep_58= ifelse(dep_id==58,1,0),
      dep_59= ifelse(dep_id==59,1,0),
      dep_60= ifelse(dep_id==60,1,0),
      dep_61= ifelse(dep_id==61,1,0),
      dep_62= ifelse(dep_id==62,1,0),
      dep_63= ifelse(dep_id==63,1,0),
      dep_64= ifelse(dep_id==64,1,0),
      dep_65= ifelse(dep_id==65,1,0),
      dep_66= ifelse(dep_id==66,1,0),
      dep_67= ifelse(dep_id==67,1,0),
      dep_68= ifelse(dep_id==68,1,0),
      dep_69= ifelse(dep_id==69,1,0)
    ) %>% 
    group_by(visitnumber) %>% 
    dplyr::summarise(
      prob_1= sum(dep_1)/n(),
      prob_2= sum(dep_2)/n(),
      prob_3= sum(dep_3)/n(),
      prob_4= sum(dep_4)/n(),
      prob_5= sum(dep_5)/n(),
      prob_6= sum(dep_6)/n(),
      prob_7= sum(dep_7)/n(),
      prob_8= sum(dep_8)/n(),
      prob_9= sum(dep_9)/n(),
      prob_10= sum(dep_10)/n(),
      prob_11= sum(dep_11)/n(),
      prob_12= sum(dep_12)/n(),
      prob_13= sum(dep_13)/n(),
      prob_14= sum(dep_14)/n(),
      prob_15= sum(dep_15)/n(),
      prob_16= sum(dep_16)/n(),
      prob_17= sum(dep_17)/n(),
      prob_18= sum(dep_18)/n(),
      prob_19= sum(dep_19)/n(),
      prob_20= sum(dep_20)/n(),
      prob_21= sum(dep_21)/n(),
      prob_22= sum(dep_22)/n(),
      prob_23= sum(dep_23)/n(),
      prob_24= sum(dep_24)/n(),
      prob_25= sum(dep_25)/n(),
      prob_26= sum(dep_26)/n(),
      prob_27= sum(dep_27)/n(),
      prob_28= sum(dep_28)/n(),
      prob_29= sum(dep_29)/n(),
      prob_30= sum(dep_30)/n(),
      prob_31= sum(dep_31)/n(),
      prob_32= sum(dep_32)/n(),
      prob_33= sum(dep_33)/n(),
      prob_34= sum(dep_34)/n(),
      prob_35= sum(dep_35)/n(),
      prob_36= sum(dep_36)/n(),
      prob_37= sum(dep_37)/n(),
      prob_38= sum(dep_38)/n(),
      prob_39= sum(dep_39)/n(),
      prob_40= sum(dep_40)/n(),
      prob_41= sum(dep_41)/n(),
      prob_42= sum(dep_42)/n(),
      prob_43= sum(dep_43)/n(),
      prob_44= sum(dep_44)/n(),
      prob_45= sum(dep_45)/n(),
      prob_46= sum(dep_46)/n(),
      prob_47= sum(dep_47)/n(),
      prob_48= sum(dep_48)/n(),
      prob_49= sum(dep_49)/n(),
      prob_50= sum(dep_50)/n(),
      prob_51= sum(dep_51)/n(),
      prob_52= sum(dep_52)/n(),
      prob_53= sum(dep_53)/n(),
      prob_54= sum(dep_54)/n(),
      prob_55= sum(dep_55)/n(),
      prob_56= sum(dep_56)/n(),
      prob_57= sum(dep_57)/n(),
      prob_58= sum(dep_58)/n(),
      prob_59= sum(dep_59)/n(),
      prob_60= sum(dep_60)/n(),
      prob_61= sum(dep_61)/n(),
      prob_62= sum(dep_62)/n(),
      prob_63= sum(dep_63)/n(),
      prob_64= sum(dep_64)/n(),
      prob_65= sum(dep_65)/n(),
      prob_66= sum(dep_66)/n(),
      prob_67= sum(dep_67)/n(),
      prob_68= sum(dep_68)/n(),
      prob_69= sum(dep_69)/n(),
      obj_abs = sum(abs(scancount)),
      num_obj =  sum(scancount),
      porc_devol = 1 - (num_obj/obj_abs),
      devol = ifelse(porc_devol<1,1,0),
      unique_prod = length(unique(finelinenumber)),
      day = mean(day),
      weekend = mean(weekend)
    ) %>% ungroup()
}

dummy_days <- function(data){
  df <- data %>% 
    mutate(
      day1 = ifelse(day==1,1,0),
      day2 = ifelse(day==2,1,0),
      day3 = ifelse(day==3,1,0),
      day4 = ifelse(day==4,1,0),
      day5 = ifelse(day==5,1,0),
      day6 = ifelse(day==6,1,0 ), 
      day7 = ifelse(day==7,1,0 )
      )
  df$day <- NULL
  return(df)
}

# Creamos los nuevos conjuntos de datos que reflejan la ingenieria de caracteristicas
walmart_train_trip <- walmart_train %>% 
  format_data %>% 
  fill_na() %>% 
  create_features() %>%
  sum_features()

walmart_test_trip <- walmart_test %>% 
  format_data %>% 
  fill_na() %>% 
  create_features() %>%
  sum_features_test()

walmart_train_trip_dummies <- walmart_train %>% 
  format_data %>% 
  fill_na() %>% 
  create_features() %>%
  sum_features() %>% dummy_days()

walmart_test_trip_dummies <- walmart_test %>% 
  format_data %>% 
  fill_na() %>% 
  create_features() %>%
  sum_features_test() %>% dummy_days()

#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Escribimos los datos trasnformados en el directorio data
write.csv(walmart_train_trip, "../../data/walmart_train_trip.csv")
write.csv(walmart_test_trip, "../../data/walmart_test_trip.csv")
write.csv(walmart_train_trip_dummies, "../../data/walmart_train_trip_dummies.csv")
write.csv(walmart_test_trip_dummies, "../../data/walmart_test_trip_dummies.csv")
