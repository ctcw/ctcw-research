library(reshape2)
library(dplyr)
library(ggplot2)
library(scales)
library(readr)


source("graphs-functional.R")

targets = read.csv("target-metrics.csv", stringsAsFactors = TRUE)
metrics = read.csv("data-metrics.csv", stringsAsFactors = TRUE)

metrics$met = as.character(metrics$Metric)
metrics$met[metrics$met == "Global Reporting Initiative+Direct greenhouse gas (GHG) emissions (Scope 1), GRI 305-1-a (formerly G4-EN15-a)"] = "Scope 1 GHG"
metrics$met[metrics$met == "Global Reporting Initiative+Indirect greenhouse gas (GHG) emissions (Scope 2), GRI 305-2 (formerly G4-EN16-a)"] = "Scope 2 GHG"
metrics$met[metrics$met == "Global Reporting Initiative+Indirect greenhouse gas (GHG) emissions (Scope 3), GRI 305-3 (formerly G4-EN17-a)"] = "Scope 3 GHG"



company = "Unilever"
company = "Accenture"



metrics = subset.df.company(company)

m.metrics = plot.netzero.scope123(company)



sum.metrics = plot.emissionreduction.scope12(company)
  

company = "Unilever"
plot.netzero.scope123(company)
plot.emissionreduction.scope12(company)
plot.energy(company)
plot.renewable.breakdown(company)

company = "Accenture"
plot.netzero.scope123(company)
write.netzero.scope123(company)
plot.emissionreduction.scope123(company)

company = "Amazon.com, Inc."
metrics = subset.df.company(company)
plot.netzero.scope123(company)
write.netzero.scope123(company)


company = "Apple Inc."
metrics = subset.df.company(company)
sum.metrics = plot.netzero.scope123(company)
write.netzero.scope123(company)
plot.emissionreduction.scope123(company)

company = "Google Inc."
metrics = subset.df.company(company)
sum.metrics = plot.netzero.scope123(company)
write.netzero.scope123(company)
plot.emissionreduction.scope123(company)


#in depth energy google

#scope 2 location based vs  market based
scope2.metrics = metrics[metrics$met == "Scope 2 GHG" | metrics$met == "Scope_2_location",]
scope2.metrics$Values = as.character(scope2.metrics$Values)
scope2.metrics$Values = as.numeric(scope2.metrics$Values)
scope2.metrics$Metric = as.character(scope2.metrics$Metric)
scope2.metrics$Metric[scope2.metrics$met=="Scope 2 GHG"] = "Scope 2 - Market based" 
scope2.metrics$Metric[scope2.metrics$Metric=="Scope_2_location"] = "Scope 2 - Location based"

p.scope2 = ggplot(scope2.metrics)+
  aes(x = Year, y = Values, fill = Metric)+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Year", y = "Scope 2 Emission estimates (tonnes CO2e)", title = paste(company, " Scope 2 emissions - Market vs Location based accounting"),sep="")+
  scale_y_continuous(label = comma)

ggsave(paste(company, "-scope2-market-location.png", sep=""), width = 8, height = 4.5)

#renewable type
renewables.metrics = metrics[metrics$met == "Renewable_energy_use_grid" | metrics$met == "Renewable_energy_use_PPA_onsite",]
renewables.metrics$Values = as.character(renewables.metrics$Values)
renewables.metrics$Values = as.numeric(renewables.metrics$Values)

p.renewables = ggplot(renewables.metrics)+
  aes(x = Year, y = Values, fill = Metric)+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Year", y = "Renewable Energy Purchased (MWH)", title = paste(company, " Renewable Energy Purchased - PPA/Onsite vs From the grid"),sep="")+
  scale_y_continuous(label = comma)

ggsave(paste(company, "-renewable-energy-purchased.png", sep=""), width = 8, height = 4.5)

#offset type
offsets.metrics = metrics[metrics$met == "Emission_reduction_renewable_PPAs" | metrics$met == "Emission_compensated_carbon_credits" | metrics$met == "Renewable_energy_use_grid" | metrics$met == "Renewable_energy_use_PPA_onsite",]
offsets.metrics$Values = as.character(offsets.metrics$Values)
offsets.metrics$Values = as.numeric(offsets.metrics$Values)

p.offsets = ggplot(offsets.metrics)+
  aes(x = Year, y = Values, fill = Metric)+
  geom_bar(stat = "identity", position = "dodge")+
  labs(x = "Year", y = "Energy Purchased (MWH) and Carbon Offsetting", title = paste(company, " Energy purchased and carbon offset agreements"),sep="")+
  scale_y_continuous(label = comma)

ggsave(paste(company, "-offset-energy-purchased.png", sep=""), width = 8, height = 4.5)


company = "Nestle"
metrics = subset.df.company(company)
sum.metrics = plot.netzero.scope123(company)
write.netzero.scope123(company)
plot.emissionreduction.scope123(company)

company = "GlaxoSmithKline"
metrics = subset.df.company(company)
sum.metrics = plot.netzero.scope123(company)
write.netzero.scope123(company)

plot.emissionreduction.scope(company, c("Scope 1 GHG", "Scope 2 GHG"), "1+2")
metrics = subset.df.company(company)
plot.emissionreduction.scope(company, c("Scope 3 GHG"), "3")


scopetype = "1+2"
plot.emissionreduction.2targets.scope(company, c("Scope 1 GHG", "Scope 2 GHG"), "1+2")
metrics = subset.df.company(company)
plot.emissionreduction.2targets.scope(company, c("Scope 3 GHG"), "3")


company = "AP Moller - Maersk"
metrics = subset.df.company(company)
sum.metrics = plot.netzero.scope123(company)
write.netzero.scope123(company)

plot.emissionreduction.scope(company, c("Scope 1 GHG", "Scope 2 GHG", "Scope 3 GHG"), "1+2+3")

#do location vs market based


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




#penalty violations Unilever

penalties = read.csv("unilever-violations.csv")
penalties$penalty.numeric = parse_number(penalties$Penalty)
penalties = penalties[penalties$Type == "environmental violation",]

sum.penalties = penalties %>%
  group_by(Year) %>%
  summarise(PenaltyTotal = sum(penalty.numeric))


xmin = min(s.target$ReferenceYear, sum.penalties$Year)
xmax = max(s.target$TargetYear)
referenceyear = s.target$ReferenceYear
referencevalue = max(sum.penalties$PenaltyTotal)
targetyear = max(s.target$TargetYear)


p.targetplot = ggplot(sum.penalties)+
  aes(x = Year, y = PenaltyTotal) +
  geom_bar(stat = "identity") +
  xlim(xmin, xmax)+
  geom_segment(x = referenceyear, xend = targetyear, y = referencevalue, yend = 0, size = 2)+
  labs(x = "Year", y = "$ Fines for Environmental Violations", title = "'Environmental Violation' data from Good Jobs First")+
  scale_y_continuous(label = comma)+
  scale_colour_manual(values = colordict)+
  scale_fill_manual(values = colordict, name = "Integrity Rating")+
  geom_vline(xintercept = PublicationYear, linetype = "dotted")

ggsave("Unilever-violations.png", width = 8, height = 4.5)
  
#for netzero plots, could do an over/under plot where we look growth/reduction of emissions and offsets


