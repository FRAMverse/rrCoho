#' assign_fram_fishery_to_crc
#'
#' @description Simple wrapper on `case_when` that associates FRAM FisheryID values according to CRC catch area codes. 
#' A character-class "area_code" field in the input object is required and must have values with a minimum of 2 characters. 
#' For example, an "areacode" of 1 should be "01", via something like `area_code = stringr::str_pad(areacode, width = 2, pad = "0")`
#' 
#' @param x data.frame-like object with character "area_code" column 
#'
#' @return object with new (or overwritten) integer column "FisheryID"
#' @export
#'
assign_fram_fishery_to_crc <- function(x, ...){
  
  if(!any(grepl("area_code", names(x)))) {
    stop("input object requires field of CRC character/string 'area_code' with minimally 2 nchar ('01')")
  }
  if(any(grepl("FisheryID", names(x)))) {
    warning("input object has 'FisheryID' field that will be overwritten")
  }
  
  dplyr::mutate(.data = x,
    FisheryID = dplyr::case_when(
      area_code == "01" ~ 33, #A1-Ast Spt
      area_code == "02" ~ 37, #Area 2 Spt
      area_code == "03" ~ 40, #Area 3 Spt
      area_code == "04" ~ 41, #Area 4 Spt
      area_code == "21" ~ 45, #Willpa Spt, marine only
      area_code %in% c(
        '305','375',
        '377','378','379',
        '382','384','389',
        '390','422','424') ~ 46,  #Wlp Tb Spt, freshwater only
      area_code == "22" ~ 48, #GryHbr Spt
      area_code %in% c('342','367') ~ 49, #SGryHb Spt, (342=Elk River, 367=Johns River)
      area_code %in% c('358','361','362') ~ 51,  #Hump R Spt, freshwater only
      area_code %in% c(
        '315','317','319',
        '325','333','370',
        '419','706','321') ~ 54, #Chehal Spt, freshwater only (?including Quigg Lake=370)
      #MarleneB: FisheryID = 58,59,60,61 - roll into FisheryID=54, to align with how pre-season inputs are calculated
      area_code == 337 ~ 54, #Wynoch Spt FisheryID 58
      area_code == 355 ~ 54, #Hoquam Spt FisheryID 59
      area_code == 335 ~ 54, #Wishkh Spt FisheryID 60
      area_code %in% c('329','331') ~ 54, #Satsop Spt FisheryID 61
      area_code %in% c(
        '327','410','412','408') ~ 62,  #Quin R Spt; no FRAM BP
      area_code == 394 ~ 65, #(Queets Spt)
      #MarleneB FisheryID = 66,67 - roll into 65, to align with how pre-season inputs are calculated
      area_code == 392 ~ 65, #Clrwtr Spt FisheryID 66
      area_code == 396 ~ 65, #Salm R Spt FisheryID 67
      area_code %in% c(
        '398','400',
        '402','404','406') ~ 70, #Quilly Spt
      area_code %in% c(
        '348','350','352') ~ 73, #Hoh R Spt
      #No FRAM BP for F76 Makah Tributary Sport
      #MarleneB: perhaps Big River (Clallam Co) = 386, Sooes River (Clallam Co) = 418, Waatch River = 420
      area_code %in% c(
        '782','822','772') ~ 89, #EJDF FWSpt no BP available
      area_code %in% c(
        '718','726','740',
        '752','768','810',
        '814','818','882') ~ 90, #WJDF FWSpt
      area_code == '05' ~ 91, #Area 5 Spt
      area_code == '06' ~ 92, #Area 6 Spt
      area_code == '07' ~ 93, #Area 7 Spt
      area_code %in% c('738','311') ~ 94, #Dung R Spt
      area_code == '742' ~ 95, #ElwhaR Spt
      
      #MarleneB: not included; 724-Dakotah Creek into 7A directly, 884-Whatcom Creek into 7B directly
      area_code %in% c(
        '788','790',
        '792','794','838') ~ 99, #Nook R Spt
      area_code == '816' ~ 100, #Samh R Spt
      area_code == '81' ~ 106, #Ar 8-1 Spt
      area_code == '09' ~ 107, #Area 9 Spt
      area_code %in% c(
        '824','825','826',
        '828','830','832') ~ 108, #Skag R Spt
      area_code == '82' ~ 115, #Ar 8-2 Spt
      area_code %in% c(
        '866','872','874','876') ~ 116,  #Stilg R Spt
      area_code %in% c(
        '840','844','846',
        '848','850','852',
        '858','860') ~ 117, #Snoh R Spt, including Wallace, Skykomish, Snoqualmie, Pilchuck
      area_code == '10' ~ 118, #Ar 10 Spt
      area_code == '746' ~ 127, #Duwm R Spt
      area_code %in% c(
        '712','760','762','764') ~ 128, #L WaSm Spt
      area_code == '11' ~ 129, #Ar 11 Spt
      area_code %in% c(
        '802','804','806','808') ~ 135, #Puyl R Spt
      area_code == '13' ~ 136, #Ar 13 Spt
      area_code %in% c(
        '708','714','720',
        '744','754','770',
        '774','776','778',
        '798','820') ~ 149, #13D-K TSpt
      area_code == '786' ~ 150, #Nisq R Spt
      area_code == '728' ~ 151, #Desc R Spt
      area_code == '12' ~ 152, #Ar 12 Spt
      area_code %in% c(
        '702','734','736','750') ~ 163, #1212B TSpt
      area_code %in% c('766','812') ~ 164, #12A Tb Spt
      area_code %in% c('732','878','880') ~ 165, #12C-D TSpt
      area_code %in% c('796','834','836') ~ 166 #Skok R Spt
    ) |> as.integer()
  )
  
}