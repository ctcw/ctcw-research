
subset.df.company = function(company){
  s.metrics = metrics[metrics$Company == company,]
  return(s.metrics)
}


#scope 1-3 target net zero
plot.netzero.scope123 = function(company){
  
  s.metrics = metrics[metrics$met == "Scope 3 GHG" | metrics$met == "Scope 2 GHG" | metrics$met == "Scope 1 GHG",]
  
  
  s.target = targets[targets$TargetType == "NetZero" & targets$Company == company,]
  
  
  m.metrics = melt(s.metrics, id.vars = c("Year", "Company", "met"), measure.vars = c("Values"))
  
  
  m.metrics$value = as.numeric(m.metrics$value)
  
  
  #quick bar chart
  p.bar = ggplot(m.metrics)+
    aes(x = Year, y = value, fill = met)+
    geom_bar(stat="identity", position="dodge")+
    labs(y="Tonnes of CO2e Emissions", title = paste("Annual GHG Emissions by Scope for", company))+
    scale_y_continuous(label = comma)
  
  ggsave(paste(company, "-emissions-scope.png", sep=""), width = 8, height = 4.5)
  
  
  sum.metrics = m.metrics %>%
    group_by(Year) %>%
    summarise(cumemissions = sum(value))
  
  
  xmin = min(s.target$ReferenceYear, m.metrics$Year)
  xmax = max(s.target$TargetYear)
  referenceyear = s.target$ReferenceYear
  referencevalue = sum.metrics$cumemissions[sum.metrics$Year == referenceyear]
  targetyear = max(s.target$TargetYear)
  #do I have reference year data? otherwise difficult to draw a line
  colordict<-c(
    "Very Low"="#800000",
    "Low"="red",
    "Moderate"="orange",
    "Reasonable"="#00994d",
    "High"="green"
  )
  

  Transparency = s.target$TargetTransparency
  Integrity = s.target$TargetIntegrity
  PublicationYear = s.target$PublishedYear

  
  targets$targetprop = targets$TargetProportion/100
  
  
  p.targetplot = ggplot(sum.metrics)+
    aes(x = Year, y = cumemissions) +
    geom_bar(stat = "identity") +
    xlim(xmin, xmax)+
    geom_point(x = referenceyear,  y = referencevalue, size = 5, aes(shape = Transparency) )+
    geom_segment(x = referenceyear, xend = targetyear, y = referencevalue, yend = 0, size = 2, aes(colour = Integrity))+
    labs(x = "Year", y = " Scope 1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Net Zero' Target assessments by CCRM", sep=""), size=0.5)+
    scale_y_continuous(label = comma)+
    scale_colour_manual(values = colordict)+
    scale_fill_manual(values = colordict, name = "Integrity Rating")+
    geom_vline(xintercept = PublicationYear, linetype = "dotted")
  #vline for publication year  
  ggsave(paste(company, "-scope-1-2-3-netzero.png", sep=""), width = 8, height = 4.5)
  
  return(p.targetplot)
}

write.netzero.scope123 = function(company){
  
  s.target = targets[targets$TargetType == "NetZero" & targets$Company == company,]
  
  referenceyear = s.target$ReferenceYear
  referencevalue = sum.metrics$cumemissions[sum.metrics$Year == referenceyear]
  targetyear = max(s.target$TargetYear)
  Transparency = s.target$TargetTransparency
  Integrity = s.target$TargetIntegrity
  PublicationYear = s.target$PublishedYear
  
  para1 = paste(company, " has published a number of targets related to carbon emissions. In the first plot we can see that they set a target of Net Zero emissions (where any emissions are offset by carbon capture or storage projects) by ", targetyear, " with a reference year of ", referenceyear, ". The target was published in ", PublicationYear, ". This target was rated ", Transparency, " on Transparency, assessing the level of disclosure for past/present emissions and the level of detail provided about planning to achieve target aims. This target was rated ", Integrity, " on Integrity, an assessment of the target's ambition and appropriateness. In these graphs the dashed line represents the target's publication date, the target line starts from the reference year.", sep="")
  return(para1)
  
}


scope = c("Scope 1 GHG", "Scope 2 GHG",  "Scope 3 GHG")
scopetype = "1+2+3"


plot.emissionreduction.scope = function(company, scope, scopetype){
  s.target = targets[targets$TargetType == "EmissionReduction" & targets$Company == company & targets$ScopeCoverage == scopetype,]
  
  
  s.metrics = metrics[metrics$met %in% scope,]
  
  m.metrics = melt(s.metrics, id.vars = c("Year", "Company", "met"), measure.vars = c("Values"))
  
  
  
  m.metrics$value = as.numeric(m.metrics$value)
  sum.metrics = m.metrics %>%
    group_by(Year) %>%
    summarise(cumemissions = sum(value))
  
  s.target$reductionfactor = s.target$TargetProportion/100
  
  s.target$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == s.target$ReferenceYear]
  s.target$TargetValue = s.target$ReferenceValue * s.target$reductionfactor
  
  #if s.target has more than 1 row, it needs a different approach
  if(nrow(s.target)> 1){
    
    
    
    p.targetplot = ggplot(sum.metrics)+
      aes(x = Year, y = cumemissions) +
      geom_bar(stat = "identity") +
      xlim(xmin, xmax)+
      geom_segment(data = s.target, size=1.5, mapping = aes(x=ReferenceYear,  xend = TargetYear, y = ReferenceValue, yend = TargetValue, colour = TemperatureAlignment))+
      labs(x = "Year", y = paste("Scope ", scopetype, " emissions (tonnes CO2e)", sep=""), title = paste(company, " 'Emission Reduction' Targets", sep=""))+
      scale_y_continuous(label = comma)+
      scale_colour_manual(values = colordict)+
      geom_vline(xintercept = PublicationYear, linetype = "dotted")
    
  }
  
  
  if(nrow(s.target) == 1){
    
    
    xmin = min(s.target$ReferenceYear, m.metrics$Year)
    xmax = max(s.target$TargetYear)
    referenceyear = s.target$ReferenceYear
    reductionfactor = s.target$reductionfactor

    
    s.target$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == referenceyear]
    targetyear = max(s.target$TargetYear)
    referencevalue = s.target$ReferenceValue
        targetvalue = referencevalue-(referencevalue*reductionfactor)
    
    #do I have reference year data? otherwise difficult to draw a line
    TemperatureAlignment = as.factor(s.target$TemperatureAlignment)
    PublicationYear = s.target$PublishedYear
    
    #get the 1.5C colour coding on here
    colordict<-c(
      "1.5"="green",
      "2"="orange"
    )
    
    
    
    p.targetplot = ggplot(sum.metrics)+
      aes(x = Year, y = cumemissions) +
      geom_bar(stat = "identity") +
      xlim(xmin, xmax)+
      geom_segment(x = referenceyear, xend = targetyear, y = referencevalue, yend = targetvalue, size = 2,  aes(colour = TemperatureAlignment))+
      labs(x = "Year", y = paste("Scope ", scopetype, " emissions (tonnes CO2e)", sep=""), "1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Emission Reduction' Targets", sep=""))+
      scale_y_continuous(label = comma)+
      scale_colour_manual(values = colordict)+
      geom_vline(xintercept = PublicationYear, linetype = "dotted")
  }
  
  ggsave(paste(company, "-scope-", scopetype, "-reduction.png", sep=""), width = 8, height = 4.5)
  
  return(sum.metrics)
  
  
}


plot.emissionreduction.2targets.scope = function(company, scope, scopetype){
  s.target = targets[targets$TargetType == "EmissionReduction" & targets$Company == company & targets$ScopeCoverage == scopetype,]
  
  
  s.metrics = metrics[metrics$met %in% scope,]
  
  m.metrics = melt(s.metrics, id.vars = c("Year", "Company", "met"), measure.vars = c("Values"))
  
  
  
  m.metrics$value = as.numeric(m.metrics$value)
  sum.metrics = m.metrics %>%
    group_by(Year) %>%
    summarise(cumemissions = sum(value))
  
  s.target$reductionfactor = s.target$TargetProportion/100
  
  s.target$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == s.target$ReferenceYear]
  s.target$TargetValue = s.target$ReferenceValue * s.target$reductionfactor
  
  #if s.target has more than 1 row, it needs a different approach
  if(nrow(s.target)> 1){
    
    
    xmin = min(s.target$ReferenceYear, m.metrics$Year)
    xmax = max(s.target$TargetYear)
    referenceyear = s.target$ReferenceYear
    reductionfactor = s.target$reductionfactor
    
    
    referencevalue = s.target$ReferenceValue
    targetvalue = referencevalue-(referencevalue*reductionfactor)
    
    #do I have reference year data? otherwise difficult to draw a line
    TemperatureAlignment = as.factor(s.target$TemperatureAlignment)
    PublicationYear = s.target$PublishedYear
    
    #get the 1.5C colour coding on here
    colordict<-c(
      "1.5"="green",
      "2"="orange"
    )
    
    
    p.targetplot = ggplot(sum.metrics)+
      aes(x = Year, y = cumemissions) +
      geom_bar(stat = "identity") +
      xlim(xmin, xmax)+
      geom_segment(data = s.target, size=1.5, mapping = aes(x=ReferenceYear,  xend = TargetYear, y = ReferenceValue, yend = TargetValue, colour = TemperatureAlignment))+
      labs(x = "Year", y = paste("Scope ", scopetype, " emissions (tonnes CO2e)", sep=""), title = paste(company, " 'Emission Reduction' Targets", sep=""))+
      scale_y_continuous(label = comma)+
      scale_colour_manual(values = colordict)+
      geom_vline(xintercept = PublicationYear, linetype = "dotted")
    
  }
  
  
  if(nrow(s.target) == 1){
    

    
    
    p.targetplot = ggplot(sum.metrics)+
      aes(x = Year, y = cumemissions) +
      geom_bar(stat = "identity") +
      xlim(xmin, xmax)+
      geom_segment(x = referenceyear, xend = targetyear, y = referencevalue, yend = targetvalue, size = 2,  aes(colour = TemperatureAlignment))+
      labs(x = "Year", y = paste("Scope ", scopetype, " emissions (tonnes CO2e)", sep=""), "1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Emission Reduction' Targets", sep=""))+
      scale_y_continuous(label = comma)+
      scale_colour_manual(values = colordict)+
      geom_vline(xintercept = PublicationYear, linetype = "dotted")
  }
  
  ggsave(paste(company, "-scope-", scopetype, "-reduction-2targets.png", sep=""), width = 8, height = 4.5)
  
  return(sum.metrics)
  
  
}



plot.emissionreduction.scope123 = function(company){
    s.target = targets[targets$TargetType == "EmissionReduction" & targets$Company == company,]
    
    
    s.metrics = metrics[metrics$met == "Scope 3 GHG" | metrics$met == "Scope 2 GHG" | metrics$met == "Scope 1 GHG",]
    
    m.metrics = melt(s.metrics, id.vars = c("Year", "Company", "met"), measure.vars = c("Values"))
    
    #get the 1.5C colour coding on here
    colordict<-c(
      "1.5"="green",
      "2"="orange"
    )
  
    
    m.metrics$value = as.numeric(m.metrics$value)
    sum.metrics = m.metrics %>%
      group_by(Year) %>%
      summarise(cumemissions = sum(value))
    
    s.target$reductionfactor = s.target$TargetProportion/100
    
    s.target$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == s.target$ReferenceYear]
    s.target$TargetValue = s.target$ReferenceValue * s.target$reductionfactor
    
    #if s.target has more than 1 row, it needs a different approach
    if(nrow(s.target)> 1){
      

    
      p.targetplot = ggplot(sum.metrics)+
        aes(x = Year, y = cumemissions) +
        geom_bar(stat = "identity") +
        xlim(xmin, xmax)+
        geom_segment(data = s.target, size=1.5, mapping = aes(x=ReferenceYear,  xend = TargetYear, y = ReferenceValue, yend = TargetValue, colour = TemperatureAlignment))+
        labs(x = "Year", y = "Scope 1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Emission Reduction' Targets as represented by Science Based Targets", sep=""))+
        scale_y_continuous(label = comma)+
        scale_colour_manual(values = colordict)+
        geom_vline(xintercept = PublicationYear, linetype = "dotted")
      
      }
    
    
    if(nrow(s.target) == 1){
      
    
    xmin = min(s.target$ReferenceYear, m.metrics$Year)
    xmax = max(s.target$TargetYear)
    referenceyear = s.target$ReferenceYear
    
    
    s.targets$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == referenceyear]
    targetyear = max(s.target$TargetYear)
    targetvalue = referencevalue-(referencevalue*reductionfactor)
    
    #do I have reference year data? otherwise difficult to draw a line
    TemperatureAlignment = as.factor(s.target$TemperatureAlignment)
    PublicationYear = s.target$PublishedYear
    

    
  
  
  p.targetplot = ggplot(sum.metrics)+
    aes(x = Year, y = cumemissions) +
    geom_bar(stat = "identity") +
    xlim(xmin, xmax)+
    geom_segment(x = referenceyear, xend = targetyear, y = referencevalue, yend = targetvalue, size = 2,  aes(colour = TemperatureAlignment))+
    labs(x = "Year", y = "Scope 1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Emission Reduction' Targets as represented by Science Based Targets", sep=""))+
    scale_y_continuous(label = comma)+
    scale_colour_manual(values = colordict)+
    geom_vline(xintercept = PublicationYear, linetype = "dotted")
    }

  ggsave(paste(company, "-scope-1-2-3-reduction.png", sep=""), width = 8, height = 4.5)
  
  return(sum.metrics)
  
  
}


  
plot.energy = function(company){
  #plot growth of fossil/renewable energy, show volume of "carbon credits" used
  
  energy.metrics = metrics[metrics$Metric == "Total_energy_use" | metrics$Metric == "Renewable_energy_use",] 
  energy.metrics$Values = as.character(energy.metrics$Values)
  energy.metrics$Values = as.numeric(energy.metrics$Values)
  
  r.energy.metrics = reshape(energy.metrics, direction = "wide",  idvar = c("Company", "Year"), timevar = "Metric", drop = c("Source", "Existing", "Checked", "met"))
  r.energy.metrics$Values.Total_energy_use = as.character(r.energy.metrics$Values.Total_energy_use)
  r.energy.metrics$Values.Total_energy_use = as.numeric(r.energy.metrics$Values.Total_energy_use)
  r.energy.metrics$Values.Renewable_energy_use = as.character(r.energy.metrics$Values.Renewable_energy_use)
  r.energy.metrics$Values.Renewable_energy_use = as.numeric(r.energy.metrics$Values.Renewable_energy_use)
  
  r.energy.metrics$nonrenewable = r.energy.metrics$Values.Total_energy_use - r.energy.metrics$Values.Renewable_energy_use
  
  m.energy.metrics = melt(r.energy.metrics, id.vars = c("Company", "Year"))
  m.energy.metrics = m.energy.metrics[m.energy.metrics$variable!="Values.Total_energy_use",]
  m.energy.metrics$Energy_Type = m.energy.metrics$variable
  m.energy.metrics$Energy_Type = as.character(m.energy.metrics$Energy_Type)
  
  m.energy.metrics$Energy_Type[m.energy.metrics$Energy_Type == "nonrenewable"] = "Non-renewable"
  m.energy.metrics$Energy_Type[m.energy.metrics$Energy_Type == "Values.Renewable_energy_use"] = "Renewable"
  
  p.energy = ggplot(m.energy.metrics)+
    aes(x=Year, y = value, fill = Energy_Type)+
    geom_bar(stat = "identity")+
    labs(x = "Year", y = "Energy used (GJ)", title = paste(company, " Energy use by year and renewable/non-renewable status"),sep="")+
    scale_y_continuous(label = comma)+
    scale_fill_manual(values = c("#e60000", "#00cc99"))
  
  ggsave(paste(company, "-energy-renewable.png", sep=""), width = 8, height = 4.5)
  return(m.energy.metrics)
}



plot.renewable.breakdown = function(company){
  
  #zoom in on renewable composition
  
  renewable.metrics = metrics[metrics$Metric == "Renewable_Purchased_Offsite" | metrics$Metric == "Renewable_Purchased_Bundled_RECs" | metrics$Metric == "Renewable_Purchased_Unbundled_RECs", ]
  
  renewable.metrics$Values = as.numeric(renewable.metrics$Values)
  
  r.renewable.metrics = reshape(renewable.metrics, direction = "wide",  idvar = c("Company", "Year"), timevar = "Metric", drop = c("Source", "Existing", "Checked", "met"))
  
  m.renewable.metrics = melt(r.renewable.metrics, id.vars = c("Company", "Year"))
  m.renewable.metrics$Energy_Source = m.renewable.metrics$variable
  m.renewable.metrics$Energy_Source = as.character(m.renewable.metrics$Energy_Source)
  
  m.renewable.metrics$Energy_Source[m.renewable.metrics$Energy_Source == "Values.Renewable_Purchased_Offsite"] = "Purchased - Offsite"
  m.renewable.metrics$Energy_Source[m.renewable.metrics$Energy_Source == "Values.Renewable_Purchased_Bundled_RECs"] = "Bundled RECs"
  m.renewable.metrics$Energy_Source[m.renewable.metrics$Energy_Source == "Values.Renewable_Purchased_Unbundled_RECs"] = "Unbundled RECs"
  
  p.energy = ggplot(m.renewable.metrics)+
    aes(x=Year, y = value, fill = Energy_Source)+
    geom_bar(stat = "identity")+
    labs(x = "Year", y = "Energy sourced (%)", title = paste(company, " Energy source by year for renewable energy only"),sep="")+
    scale_y_continuous(label = comma)+
    scale_fill_discrete()
  
  ggsave(paste(company, "-renewable-sourcing.png", sep=""), width = 8, height = 4.5)
  return(m.renewable.metrics)
  
  
}



plot.emissionreduction.scope12 = function(company){
  s.target = targets[targets$TargetType == "EmissionReduction" & targets$Company == company & targets$ScopeCoverage == "1+2",]
  
  
  
  s.metrics = metrics[metrics$met == "Scope 3 GHG" | metrics$met == "Scope 2 GHG" | metrics$met == "Scope 1 GHG",]
  
  m.metrics = melt(s.metrics, id.vars = c("Year", "Company", "met"), measure.vars = c("Values"))
  
  
  
  m.metrics$value = as.numeric(m.metrics$value)
  sum.metrics = m.metrics %>%
    group_by(Year) %>%
    summarise(cumemissions = sum(value))
  
  s.target$reductionfactor = s.target$TargetProportion/100
  
  s.target$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == s.target$ReferenceYear]
  s.target$TargetValue = s.target$ReferenceValue * s.target$reductionfactor
  
  #if s.target has more than 1 row, it needs a different approach
  if(nrow(s.target)> 1){
    
    
    
    p.targetplot = ggplot(sum.metrics)+
      aes(x = Year, y = cumemissions) +
      geom_bar(stat = "identity") +
      xlim(xmin, xmax)+
      geom_segment(data = s.target, size=1.5, mapping = aes(x=ReferenceYear,  xend = TargetYear, y = ReferenceValue, yend = TargetValue, colour = TemperatureAlignment))+
      labs(x = "Year", y = "Scope 1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Emission Reduction' Targets as represented by Science Based Targets", sep=""))+
      scale_y_continuous(label = comma)+
      scale_colour_manual(values = colordict)+
      geom_vline(xintercept = PublicationYear, linetype = "dotted")
    
  }
  
  
  if(nrow(s.target) == 1){
    
    
    xmin = min(s.target$ReferenceYear, m.metrics$Year)
    xmax = max(s.target$TargetYear)
    referenceyear = s.target$ReferenceYear
    
    
    s.targets$ReferenceValue = sum.metrics$cumemissions[sum.metrics$Year == referenceyear]
    targetyear = max(s.target$TargetYear)
    targetvalue = referencevalue-(referencevalue*reductionfactor)
    
    #do I have reference year data? otherwise difficult to draw a line
    TemperatureAlignment = as.factor(s.target$TemperatureAlignment)
    PublicationYear = s.target$PublishedYear
    
    #get the 1.5C colour coding on here
    colordict<-c(
      "1.5"="green",
      "2"="orange"
    )
    
    
    
    p.targetplot = ggplot(sum.metrics)+
      aes(x = Year, y = cumemissions) +
      geom_bar(stat = "identity") +
      xlim(xmin, xmax)+
      geom_segment(x = referenceyear, xend = targetyear, y = referencevalue, yend = targetvalue, size = 2,  aes(colour = TemperatureAlignment))+
      labs(x = "Year", y = "Scope 1 + 2 + 3 emissions (tonnes CO2e)", title = paste(company, " 'Emission Reduction' Targets as represented by Science Based Targets", sep=""))+
      scale_y_continuous(label = comma)+
      scale_colour_manual(values = colordict)+
      geom_vline(xintercept = PublicationYear, linetype = "dotted")
  }
  
  ggsave(paste(company, "-scope-1-2-3-reduction.png", sep=""), width = 8, height = 4.5)
  
  return(sum.metrics)
  
  
}



plot.energy = function(company){
  #plot growth of fossil/renewable energy, show volume of "carbon credits" used
  
  energy.metrics = metrics[metrics$Metric == "Total_energy_use" | metrics$Metric == "Renewable_energy_use",] 
  energy.metrics$Values = as.character(energy.metrics$Values)
  energy.metrics$Values = as.numeric(energy.metrics$Values)
  
  r.energy.metrics = reshape(energy.metrics, direction = "wide",  idvar = c("Company", "Year"), timevar = "Metric", drop = c("Source", "Existing", "Checked", "met"))
  r.energy.metrics$Values.Total_energy_use = as.character(r.energy.metrics$Values.Total_energy_use)
  r.energy.metrics$Values.Total_energy_use = as.numeric(r.energy.metrics$Values.Total_energy_use)
  r.energy.metrics$Values.Renewable_energy_use = as.character(r.energy.metrics$Values.Renewable_energy_use)
  r.energy.metrics$Values.Renewable_energy_use = as.numeric(r.energy.metrics$Values.Renewable_energy_use)
  
  r.energy.metrics$nonrenewable = r.energy.metrics$Values.Total_energy_use - r.energy.metrics$Values.Renewable_energy_use
  
  m.energy.metrics = melt(r.energy.metrics, id.vars = c("Company", "Year"))
  m.energy.metrics = m.energy.metrics[m.energy.metrics$variable!="Values.Total_energy_use",]
  m.energy.metrics$Energy_Type = m.energy.metrics$variable
  m.energy.metrics$Energy_Type = as.character(m.energy.metrics$Energy_Type)
  
  m.energy.metrics$Energy_Type[m.energy.metrics$Energy_Type == "nonrenewable"] = "Non-renewable"
  m.energy.metrics$Energy_Type[m.energy.metrics$Energy_Type == "Values.Renewable_energy_use"] = "Renewable"
  
  p.energy = ggplot(m.energy.metrics)+
    aes(x=Year, y = value, fill = Energy_Type)+
    geom_bar(stat = "identity")+
    labs(x = "Year", y = "Energy used (GJ)", title = paste(company, " Energy use by year and renewable/non-renewable status"),sep="")+
    scale_y_continuous(label = comma)+
    scale_fill_manual(values = c("#e60000", "#00cc99"))
  
  ggsave(paste(company, "-energy-renewable.png", sep=""), width = 8, height = 4.5)
  return(m.energy.metrics)
}
 