generate_sunburst_df<-function(background_df,variables_list,user_data_analysis,sig_t,tol_1, tol_2,equity_metric_selected, whether_NMI){
  
  num_variables_df<-length(variables_list)
  background_info_df<- background_df %>%
    group_by(.dots = variables_list[1:num_variables_df]) %>% 
    summarise(background_n = sum(background_n))
  background_info_df<-na.omit(background_info_df)
  total_population_background<- sum(background_info_df$background_n)
  background_info_df$total_background<-rep(total_population_background, nrow(background_info_df))
  
  user_info_df<- user_data_analysis %>%
    group_by(.dots = variables_list[1:num_variables_df]) %>% 
    summarise(user_n = sum(user_n))
  user_info_df<-na.omit(user_info_df)
  total_population_user<- sum(user_info_df$user_n)
  user_info_df$total_user<-rep(total_population_user, nrow(user_info_df))
  
  participant_rate<-total_population_user/total_population_background
  
  index_max<-num_variables_df-1
  
  if (index_max>0){
    
    for (var_index in index_max:1){
      background_var<-background_df %>%
        group_by(.dots = variables_list[1:var_index]) %>% 
        summarise(background_n = sum(background_n))
      background_var<-na.omit(background_var)
      
      user_info_df_var<-user_data_analysis%>%
        group_by(.dots = variables_list[1:var_index])%>%
        summarise(user_n = sum(user_n))
      user_info_df_var<-na.omit(user_info_df_var)
      
      total_population_user_var<-sum(as.integer(user_info_df_var$user_n))
      total_population_background_var<-sum(as.integer(background_var$background_n))
      
      background_var$total_background<-rep(total_population_background_var, nrow(background_var))
      user_info_df_var$total_user<-rep(total_population_user_var, nrow(user_info_df_var))
      
      background_info_df<-plyr::rbind.fill(background_info_df, background_var) 
      user_info_df<-plyr::rbind.fill(user_info_df, user_info_df_var) 
    }
    
  }
  
  
  #combine user input data with the background info
  merged_df<-merge(background_info_df, user_info_df, by=1:(ncol(background_info_df)-2), all=TRUE)
  
  
  merged_df$user_n<-merged_df$user_n %>% replace_na(0)
  merged_df$background_n<-merged_df$background_n %>% replace_na(0)
  
  merged_df$total_user<-merged_df$total_user %>% replace_na(0)
  merged_df$total_background<-merged_df$total_background %>% replace_na(0)
  
  #calculate the observed & background rates 
  merged_df$Observed_Rate<- mapply(Rate_Calculation,as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$Background_Rate<- mapply(Rate_Calculation,as.integer(merged_df$background_n),as.integer(merged_df$total_background))
  
  merged_df$pValue<- mapply(compare_population_proportion,as.numeric(merged_df$Background_Rate),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- p.adjust(merged_df$pValue,method = "BH")
  
  if (whether_NMI){
    merged_df$EquityValue<- mapply(equity_metric_selected,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate),as.numeric(participant_rate), for_plot = FALSE)
    merged_df$EquityLable<-mapply(whether_biased_label_NMI, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2)
    merged_df$EquityColors<-mapply(add_colors_NMI, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2)
  }
  else{
    merged_df$EquityValue<- mapply(equity_metric_selected,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
    merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
    merged_df$EquityColors<-mapply(add_colors, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  }
  merged_df$na_count <- apply(merged_df[,1:num_variables_df,drop=F], 1, function(x) sum(is.na(x)))
  
  num_groups<-nrow(merged_df)
  
  new_df<- data.frame(ids = character(num_groups),
                      labels = character(num_groups),
                      parents = character(num_groups),
                      Observed_Rate = merged_df$Observed_Rate,
                      Ideal_Rate = merged_df$Background_Rate,
                      EquityColors=merged_df$EquityColors,
                      EquityLable=merged_df$EquityLable,
                      EquityValue=merged_df$EquityValue,
                      Observed_Number=merged_df$user_n,
                      Trial_Number = merged_df$total_user,
                      stringsAsFactors=FALSE)
  
  for (group_index in 1:num_groups){
    level<-num_variables_df-merged_df[group_index,'na_count']
    if (level>1){
      row_list <- unlist(merged_df[group_index,1:level])
      id_name<- paste(row_list,collapse=" - ")
      label_name<- as.character(row_list[level])
      parent_name<-paste(row_list[1:(level-1)],collapse=" - ")
    }
    else{
      id_name<- as.character(merged_df[group_index,1])
      label_name<- id_name
      parent_name<-""
    }
    
    new_df[group_index,'ids']<-id_name
    new_df[group_index,'labels']<-label_name
    new_df[group_index,'parents']<-parent_name
    
  }
  
  new_df$EquityValue<- mapply(signif,  new_df$EquityValue,digits = 3)
  new_df$Observed_Rate<- mapply(signif,  new_df$Observed_Rate,digits = 3)
  new_df$Ideal_Rate<- mapply(signif,  new_df$Ideal_Rate,digits = 3)
  new_df$TextColors<- mapply(colorpicker_text,new_df$EquityColors)
  return(new_df)
}


colorpicker_text <- function(z){
  if(z == "#54585a"){return("white")}
  else if(z == "#9ea2a2"){return("white")}
  else if(z == "#ab2328"){return("white")}
  else {return("black")}
}


generate_sunburst_plotly<-function(new_df, given_width = NULL){ 
  plot_ly(data= new_df,source = "sunSource")%>% add_trace(
    type='sunburst',
    ids=new_df$ids,
    labels=new_df$labels,
    parents=new_df$parents,
    leaf = list(opacity = 1),
    marker = list(colors = new_df$EquityColors),
    textfont = list(color = new_df$TextColors),
    #textinfo = "current path+label",
    maxdepth =-1,
    insidetextorientation='radial',
    hoverlabel = list(font = list(color = new_df$TextColors)),
    #hoverinfo = "current path+label+text",
    hovertext = ~paste(new_df$EquityLable,'</br>','Ideal Rate:', formattable(new_df$Ideal_Rate, digits = 3, format = "f"),'</br>','Observed Rate:', formattable(new_df$Observed_Rate, digits = 3, format = "f"),'</br>','No. of Participants:', new_df$Observed_Number),
    insidetextfont  = 2)%>% 
    layout(
      grid = list(columns = 1, rows = 1),
      width = given_width, height = given_width
    )
  
}

generate_sunburst_plotly2<-function(new_df, given_width = NULL){ 
  plot_ly(data= new_df,source = "sunSource")%>% add_trace(
    type='sunburst',
    ids=new_df$ids,
    labels=new_df$labels,
    parents=new_df$parents,
    leaf = list(opacity = 1),
    marker = list(colors = new_df$EquityColors),
    maxdepth =-1,
    insidetextorientation='radial',
    hovertext = ~paste(new_df$EquityLable,'</br>','Ideal Rate:', formattable(new_df$Ideal_Rate, digits = 3, format = "f"),'</br>','Observed Rate:', formattable(new_df$Observed_Rate, digits = 3, format = "f"),'</br>','No. of Participants:', new_df$Observed_Number ),
    insidetextfont  = 2)%>% 
    layout(
      grid = list(columns = 1, rows = 1),
      width = given_width, height = given_width
    )
  
}



segments_calculation<-function(l1,l2,u1,u2,given_p, function_real,seg_length){
  x<- seq(0,1,seg_length)
  find_l1 <- TRUE
  find_l2 <- TRUE
  find_u1 <- TRUE
  find_u2 <- TRUE
  a<-c(0,0,0,0)
  for (i in x){
    if (function_real(given_p, i, TRUE) >= l1 & find_l1 ){
      a[1]<-i-seg_length
      find_l1 = FALSE
    }
    else if (function_real(given_p, i, TRUE) >= l2& find_l2){
      find_l2 = FALSE
      a[2]<-i-seg_length
    }
    else if (function_real(given_p, i, TRUE) >= u1& find_u1){
      find_u1 = FALSE
      a[3]<-i-seg_length
    }
    else if (function_real(given_p, i, TRUE) >= u2& find_u2){
      find_u2 = FALSE
      a[4]<-i-seg_length
    }
    
  }
  return(a)
}


sun_plot_process<-function(function_name,subgroup_detail,function_real,cut1,cut2,cut3,cut4,sig_cut,alpha_subgroup,beta_subgroup,whether_equitable = '',seg_length, ylimit = TRUE, given_xlimit, given_ylimit){
  
  if (alpha_subgroup == 0){
    print("Insufficient Background Information !")
    return(NULL)
  }
  
  x_subgroup <- seq(0,1,seg_length)
  
  segs<-segments_calculation(cut1,cut2,cut3,cut4,alpha_subgroup,function_real,seg_length)
  
  y_subgroup<-function_real(alpha_subgroup,x_subgroup,TRUE)
  
  if ((given_ylimit==c(0,0))[1] & (given_ylimit==c(0,0))[2]  ){
    if (ylimit){
      given_ylimit <-c(-5,5)
    }
    else{
      given_ylimit <-c(-5,10)
    }
  }
  
  
  
  plot(x_subgroup,y_subgroup,
       main=paste0(function_name),
       ylab="\u03C4 = Metric Value",xlab ="Observed Rate",
       type="l",col="black",lwd = 1.5,xlim=given_xlimit,ylim=given_ylimit)
  
  
  
  x_vert <-segs[1]/seg_length
  x_vert2<-segs[2]/seg_length
  x_vert3<-segs[3]/seg_length
  x_vert4<-segs[4]/seg_length
  
  last_index<-1/seg_length
  
  y_limit_user<-floor(min(y_subgroup[2:last_index]))
  polygon(x = c(x_subgroup[1:x_vert],x_subgroup[x_vert], 0), 
          y = c(y_subgroup[1:x_vert],y_limit_user,y_limit_user),
          col = "#d58570")
  polygon(x = c(x_subgroup[x_vert:x_vert2],x_subgroup[x_vert2], x_subgroup[x_vert]), 
          y = c(y_subgroup[x_vert:x_vert2],y_limit_user,y_limit_user ),
          col = "#eabcad")
  polygon(x = c(x_subgroup[x_vert2:x_vert3],x_subgroup[x_vert3], x_subgroup[x_vert2]), 
          y = c(y_subgroup[x_vert2:x_vert3],y_limit_user,y_limit_user ),
          col = "#d4e6e8")
  polygon(x = c(x_subgroup[x_vert3:x_vert4],x_subgroup[x_vert4], x_subgroup[x_vert3]), 
          y = c(y_subgroup[x_vert3:x_vert4],y_limit_user,y_limit_user ),
          col = "#a5b0cb")
  polygon(x = c(x_subgroup[x_vert4:last_index],1, x_subgroup[x_vert4]), 
          y = c(y_subgroup[x_vert4:last_index],y_limit_user,y_limit_user ),
          col = "#667ba2")
  
  if (whether_equitable == "Equitable(p)"){
    abline(v = x_subgroup[round((x_vert2+x_vert3)/2)],col= "#878080", lwd=3, lty=2)
    text(x_subgroup[x_vert4]+0.05,1, "Subgroup\n(Cohort,p)",cex=1, col = "chocolate4" )
  }
  else{
    abline(v = beta_subgroup,col= "chocolate4", lwd=3, lty=2)
    text(beta_subgroup+0.05,2, "Subgroup\n(Cohort)",cex=1, col = "chocolate4")
  }
  
  abline(v = alpha_subgroup,col= "green4", lwd=3, lty=2)
  text(alpha_subgroup+0.05,1, "Ideal",cex=1, col = "green4")
  
  # x_subgroup2 <- seq(0,1,seg_length)
  # 
  # y_subgroup2<-Adjusted_Equal_Opportunity(alpha_subgroup,x_subgroup2,TRUE)
  # 
  # lines(x_subgroup2, y_subgroup2, col="deeppink1", lty=2,lwd = 3)
  # text(0.4+0.05,0.5, "Normalized Parity",cex=1.2, col = "deeppink1")

  
  legend("bottomright",inset=0.01, title="Representativeness Levels",
         c("Subgroup Absent in Target Population",
           "Subgroup Absent in \nTarget Population & Cohort",
           "Subgroup Absent in Cohort",
           paste0("Highly Underrepresented (\u03C4<",round(cut1,2),")"),
           paste0("Underrepresented (",round(cut1,2)," \u2264 \u03C4<",round(cut2,2),")"),
           paste0("Representative (",round(cut2,2),"\u2264 \u03C4<",round(cut3,2),"\n or p>",round(sig_cut,2),")"),
           paste0("Overrepresented (",round(cut3,2),"\u2264 \u03C4<",round(cut4,2),")"),
           paste0("Highly Overrepresented (",round(cut4,2),"\u2264\u03C4)")),
         fill=c("#54585a","#9ea2a2","#ab2328","#d58570","#eabcad","#d4e6e8","#a5b0cb","#667ba2"), horiz=FALSE, cex=0.75)
 
}





generate_sunburst_df2<-function(background_df,variables_list,user_data_analysis,sig_t,tol_1, tol_2,equity_metric_selected){
  num_variables_df<-length(variables_list)
  
  background_info_df<- background_df %>%
    group_by(.dots = variables_list[1]) %>% 
    summarise(background_n = sum(background_n))
  background_info_df<-na.omit(background_info_df)
  total_population_background<- sum(background_info_df$background_n)
  background_info_df$total_background<-rep(total_population_background, nrow(background_info_df))
  
  user_info_df<- user_data_analysis %>%
    group_by(.dots = variables_list[1]) %>% 
    summarise(user_n = sum(user_n))
  user_info_df<-na.omit(user_info_df)
  total_population_user<- sum(user_info_df$user_n)
  user_info_df$total_user<-rep(total_population_user, nrow(user_info_df))
  
  participant_rate<-total_population_user/total_population_background
  merged_df<-merge(background_info_df, user_info_df, by=1, all=TRUE)
  colnames(merged_df)[1]<-"ids"
  
  
  if (num_variables_df>1){
    for (var_index in 2:num_variables_df){
      background_var<-background_df %>%
        group_by(.dots = variables_list[var_index]) %>% 
        summarise(background_n = sum(background_n))
      background_var<-na.omit(background_var)
      
      user_info_df_var<-user_data_analysis%>%
        group_by(.dots = variables_list[var_index])%>%
        summarise(user_n = sum(user_n))
      user_info_df_var<-na.omit(user_info_df_var)
      
      total_population_user_var<-sum(as.integer(user_info_df_var$user_n))
      total_population_background_var<-sum(as.integer(background_var$background_n))
      
      background_var$total_background<-rep(total_population_background_var, nrow(background_var))
      user_info_df_var$total_user<-rep(total_population_user_var, nrow(user_info_df_var))
      
      
      merged_df_new<-merge(background_var, user_info_df_var, by=1, all=TRUE)
      colnames(merged_df_new)[1]<-"ids"
      merged_df<-rbind(merged_df, merged_df_new) 
    }
    
  }
  
  background_info_df<- background_df %>%
    group_by(.dots = variables_list[1:num_variables_df]) %>% 
    summarise(background_n = sum(background_n))
  background_info_df<-na.omit(background_info_df)
  total_population_background<- sum(background_info_df$background_n)
  background_info_df$total_background<-rep(total_population_background, nrow(background_info_df))
  
  user_info_df<- user_data_analysis %>%
    group_by(.dots = variables_list[1:num_variables_df]) %>% 
    summarise(user_n = sum(user_n))
  user_info_df<-na.omit(user_info_df)
  total_population_user<- sum(user_info_df$user_n)
  user_info_df$total_user<-rep(total_population_user, nrow(user_info_df))
  df_current_patient<-merge(background_info_df, user_info_df, by=1:(ncol(background_info_df)-2), all=TRUE)
  
  if (num_variables_df>1){
    for (group_index in 1:nrow(df_current_patient)){
      row_list <- unlist(df_current_patient[group_index,1:num_variables_df])
      id_name<- paste(row_list,collapse=" - ")
      merged_df<- merged_df%>% add_row(ids = id_name, background_n = as.numeric(df_current_patient[group_index,"background_n"]), total_background= as.numeric(df_current_patient[group_index,"total_background"]), 
                                       user_n=as.numeric(df_current_patient[group_index,"user_n"]), total_user = as.numeric(df_current_patient[group_index,"total_user"]))
      
    }
  }

  
  merged_df$user_n<-merged_df$user_n %>% replace_na(0)
  merged_df$background_n<-merged_df$background_n %>% replace_na(0)
  
  merged_df$total_user<-merged_df$total_user %>% replace_na(0)
  merged_df$total_background<-merged_df$total_background %>% replace_na(0)
  
  #calculate the observed & background rates 
  merged_df$Observed_Rate<- mapply(Rate_Calculation,as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$Background_Rate<- mapply(Rate_Calculation,as.integer(merged_df$background_n),as.integer(merged_df$total_background))
  
  merged_df$pValue<- mapply(compare_population_proportion,as.numeric(merged_df$Background_Rate),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- p.adjust(merged_df$pValue,method = "BH")
  
  merged_df$EquityValue<- mapply(equity_metric_selected,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
  merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  merged_df$EquityColors<-mapply(add_colors, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)

  num_groups<-nrow(merged_df)
  
  new_df<- data.frame(ids = merged_df$ids ,
                      Observed_Rate = merged_df$Observed_Rate,
                      Ideal_Rate = merged_df$Background_Rate,
                      EquityColors=merged_df$EquityColors,
                      EquityLable=merged_df$EquityLable,
                      EquityValue=merged_df$EquityValue,
                      ParticipantRate=rep(participant_rate, num_groups),
                      stringsAsFactors=FALSE)

  
  return(new_df)
}


