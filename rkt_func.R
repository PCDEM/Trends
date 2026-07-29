rkt_func <- function(data, location, tsfreq, endyr){
  
  # DESCRIPTION-----
  # This program takes in water quality data and information about the location 
  # such as WBID/station number and conducts a seasonal mann-kendall trend test.
  # Most of the code below was developed by Janicki Environmental Inc. for the 
  # Pinellas County Division of Environmental Management water quality monitoring
  # program. Only the first section for reformatting the water quality data was
  # developed in-house to satisfy specific trend analysis formatting requirements.
  
  # USAGE------
  #                rkt_func(data = 'path/to/water_quality_data.xlsx',
  #                         location = 'path/to/location_data.sas7bdat',
  #                         tsfreq = 8,
  #                         endyr = 2024)
  
  # INPUTS-----
  # data      = PCDEM water quality data file
  # location  = PCDEM site location data file
  # tsfreq    = Time steps for the trend test based on sampling period (i.e., 8)
  # endyr     = End year for the trend analysis (based on previous 10 years of data)
  
  # OUTPUTS----
  # 1) A table of results for each parameter at each site and level that indicates
  #    if there is a significant trend, whether it is increasing or decreasing, 
  #    and the magnitude of the trend.
  
  # AUTHORS-----
  # Mike Wessel (8/11/2021)
  # Alex Manos  (9/26/2024)
  
  #-----------------------------------------------------------------------------

  # Load packages:
  if (!nzchar(system.file(package = "librarian")))
    install.packages("librarian")
  
  librarian::shelf(
    librarian, tidyverse, haven, rkt, here, forecast, pracma, xts, lmtest, EnvStats, 
    imputeTS, car, foreign, openxlsx, readxl
  )
  
  
  # Tidy data into format needed for trends analysis:
  data <- data.frame(
    read_excel(dataPath, guess_max = 50000)) |>
    mutate(group = ifelse(month(Date) == 1 & Period == '08', year(Date) -1 , 
                          year(Date)),
           group = as.Date(paste0(group,'-01-01'))) |>
    filter(year(group) %in% (endyr-9):endyr) |>
    dplyr::mutate(
      Year = lubridate::year(group),
      TN = TKN + NOX, 
      Period = as.numeric(Period),
      Date = as.Date(Date), 
      Site = ifelse(substr(Site,1,1) %in% c('E','W','S','L','R'), substr(Site,1,2),Site)
    ) |>
    dplyr::select(
      station = Site, 
      Level, 
      Year, 
      sample_period = Period, 
      Date, 
      Conductivity = 'Sp.Conductivity',
      DO, 
      'DO%' = DO., 
      Salinity, 
      pH, 
      BOD5, 
      Chl_a = 'Chl.a', 
      E_coli, 
      Color, 
      Enterococci,
      NH3, NOX, OP, TKN, TN, TP, TSS, Turbidity
    ) |>
    tidyr::pivot_longer(
      cols = -c(station, Level, Year, sample_period, Date), 
      names_to='Param', 
      values_to='Value'
    ) |>
    group_by(station) |>
    arrange(station,Param)
  
  # Make it so that each site has a surface and bottom value for each parameter.
  # If a site only has surface, duplicate the values and call them bottom. This
  # is done because the trends loop later filters by level and needs surface and
  # bottom.
  data2 <- data |>
    group_by(station, Date) |>
    nest() |>
    mutate(data = map(data, ~ {
      # Extract surface and bottom data
      surface_data <- .x %>% filter(Level == "Surface")
      bottom_data <- .x %>% filter(Level == "Bottom")
      
      # If no bottom data, duplicate surface data and change Level to "Bottom"
      if (nrow(bottom_data) == 0) {
        bottom_data <- surface_data
        bottom_data$Level <- "Bottom"
      }
      
      # If fewer bottom data, duplicate rows to match the number of surface data
      if (nrow(bottom_data) < nrow(surface_data)){
        bottom_data <- bottom_data |>
          slice(rep(1:n(), length.out = nrow(surface_data)))
      }
      
      # Combine surface and (adjusted) bottom data
      bind_rows(surface_data, bottom_data)
    })) |>
    unnest(cols = c(data)) |>
    arrange(station, Date, Level)
  
  
  # For each site and each param in r3, remove the earliest NA rows until the first
  # data point is reached:
  data <- data2 |>
    group_by(station, Param) %>%
    mutate(first_data = which(!is.na(Value))[1]) %>%
    filter(row_number() >= first_data) %>%
    ungroup() %>%
    select(-first_data) |>
    filter(! (Level == 'Bottom' & Param %in% c('TN','OP','TP','TSS','Turbidity',
                                               'Chl_a','E_coli','Enterococci','Color',
                                               'NH3','NOX','TKN','TN','BOD5'))) |>
    filter(! station %in% c('35-04','53-05')) |>
    group_by(station,Date, Level, Year,sample_period, Param) |>
    summarise(Value = median(Value, na.rm = TRUE)) |>
    ungroup()
  
  
  
  ##############################################
  #   Wrangle full dataset to prep for analysis
  ##############################################
  
  # reassign some variable names
  mut<-data %>%
    mutate(mon=sample_period, Station = as.factor(station),lvl=as.factor(Level), 
           yr=Year, value=Value)
  
  # takes monthly median value
  med<-mut %>%
    arrange(Station,Param,yr,mon,lvl)%>%
    group_by(Station,Param,yr,mon,lvl)%>%
    summarize(med_val=median(value,na.action=na.pass)) # allow NA's to pass
  
  # produces count for nonmissing values
  cnt<-med%>%
    filter(!is.na(med_val))%>%
    group_by(Station,Param,lvl)%>%
    summarize(c.nomiss=n(),grand_med=median(med_val,na.action=na.rm))%>%
    droplevels()
  
  
  # inclusion criteria require 60 non missing values
  subit<-full_join(med,cnt,by=c("Station","Param","lvl"))%>%
    filter(c.nomiss>59)%>%
    droplevels()
  
  
  ready<-subit%>%
    mutate(value=med_val)%>%
    arrange(Station,Param,yr,mon,lvl)
  
  
  # make lists and dataframes to store results;
  df_total = data.frame()
  n.doit = data.frame()
  datalist = list()
  
  lvl_loc <- levels(ready$lvl)
  sta_loc <- levels(ready$Station)
  sta_loctib<-tibble(sta_loc)
  
  
  #####################################################
  ### begin analysis using a series of nested for loops
  ### You need to highlight entire code block enclosing
  ### the "END LOOPS" comment
  #####################################################
  
  #pick level first
  for(k in 1:length(lvl_loc)){
    bylvl<-ready%>%
      filter(lvl==lvl_loc[[k]])
    
    ## then pull a station from level;
    for(j in 1:length(sta_loc)){
      TN<-bylvl%>%
        filter(Station==sta_loc[[j]])
      
      
      ########################################################################
      ## Perform seasonal kendall trend test using rkt package and a for loop
      ########################################################################
      
      # Make list of params within station and level
      stas <- unique(TN$Param)
      staname<-tibble(stas)
      
      # make list frame for results
      res <- data.frame(matrix(nrow = length(stas), ncol = 4))
      colnames(res)<-c("pval","slope","pval.corr","tau")
      
      # process by param, perform SKT, and output results to res
      for(i in 1:length(stas)){
        doit<-subset(TN,Param==stas[[i]])
        mod <- as.data.frame(unlist(rkt(doit$yr,doit$med_val,doit$mon,,TRUE,rep="m")))
        res[i,1]<- mod[1,]
        res[i,2]<- mod[3,]
        res[i,3]<- mod[5,]
        res[i,4]<- mod[12,]
        
      }
      
      ## returns  na's for corrected pvals due to [1 block with less than 4 points ignored]
      
      ##################################################
      # detect autocorrelation
      #################################################
      DW <- rep(NA, length(stas))
      autocorr <- rep(NA, length(stas))
      
      for(i in 1:length(stas)){
        doit          <-subset(TN,Param==stas[[i]])
        doit          <-na_interpolation(doit) #interpolate missing values for autocorr test only
        doit_ts       <- ts(doit$med_val,frequency=tsfreq) #assign as timeseries with frequency interval
        fit.cr        <- stl(doit_ts,s.window="period") #use residuals from deseasonal and detrended timeseries
        fit.cr2       <-as.data.frame(fit.cr$time.series)#convert to dataframe to extract
        test          <-serialCorrelationTest(fit.cr2$remainder, test = "AR1.mle") #this may have increased false positive due to asymptotic test
        autocorr[[i]] <-test$p.value
        DW[[i]]       <-durbinWatsonTest(fit.cr2$remainder) #DW may be more appropriate for small sample size but results are different. Dont use
        
      }
      
      # gather results
      aucor<-data.frame(autocorr)
      colnames(aucor)<-"Autocorr.p"
      
      DWW<-data.frame(DW)
      colnames(DWW)<-"DW"
      
      ## create dataframe of all results
      TN_results<-cbind(staname,res,aucor,DWW)
      
      ########################################################################
      ### assign pvalue based on autocorr results
      ### use uncorrected p where autocorr failed to reject
      ### Apply Benjamini and Hochberg correction for multiple comparisons
      ########################################################################
      TN_result2<-TN_results%>%
        mutate(corrqual=ifelse(Autocorr.p<0.05,1,0),
               padj=ifelse(corrqual==1,pval.corr,pval),
               padj=ifelse(!is.na(padj),padj,pval),
               p.FDR=p.adjust(padj,"BH"),
               raw_td  =  ifelse(padj<0.05,"Trend","No Trend"),
               final_td = ifelse(p.FDR<0.05,"Trend","No Trend"),
               final_dir = ifelse(slope<0,"Decreasing","Not"),
               final_dir = ifelse(slope>0,"Increasing",final_dir),
               final_est = ifelse(final_td=="Trend",final_dir,"Not"),
               final_stat = as.factor(sta_loc[[j]]),
               final_lvl  = as.factor(lvl_loc[[k]]),
        )%>%
        arrange(stas)
      
      
      df_total <- rbind(df_total,TN_result2)
      
    }
    
  }
  
  ##############################
  ###   End loops            ###
  ##############################
  
  ### NOTE - THERE ARE CASES WHERE LESS THAN 4 DATA POINTS EXIST IN A PARTICULAR BLOCK (SAMPLE PERIOD)
  ###        WHICH THROWS A WARNING. IN THESE CASES THE BLOCK IS IGNORED
  
  
  df_total <- arrange(df_total,stas,final_stat,final_lvl)%>%ungroup()
  
  
  #add in nobs
  ndoit2<-cnt%>%
    filter(c.nomiss>59)%>%
    mutate(stas = Param,final_stat = as.factor(Station),final_lvl=lvl, counts = c.nomiss)%>%
    droplevels()%>% ungroup%>%
    arrange(stas,final_stat,final_lvl)
  
  
  # do two joins here to get 3 datasets merged since it doesn't seem to like joining three
  
  df_total2<-left_join(df_total,ndoit2)%>%  #,by=c(stas,final_stat)
    mutate(rel_val = slope/grand_med,
           rel_mag = ifelse(rel_val>0.10,"Large","Small"))%>%  # calculate relative magnitude of slope
    arrange(final_stat)
  
  
  df_total3<-full_join(df_total2,location)
  return(df_total3)
}
