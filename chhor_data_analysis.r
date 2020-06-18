#install packages
library(tidyverse);

#import and tidy data
main <- read_csv("chhor_data.csv");
main2 <- main %>% pivot_longer(c(LPS, 'IL-1Beta', TNFalpha, 'IL-4', 'IL-10', IFNnu), 
                               names_to="cytokine", values_to="quantification");

#*******************************************************************************GENERAL PLOTS
#general graph biomarker expression v exposure across many cytokines
ggplot(main2) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                         position = "dodge", stat = "identity") + 
  ggtitle("Effect of Cytokines on Biomarker Expression");

general2 <- ggplot(main2) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                         position = "stack", stat = "identity") + 
  ggtitle("Effect of Cytokines on Biomarker Expression");

#general graph biomarker expression when exposed to LPS
main_lps <- main2 %>% filter(cytokine == "LPS");
ggplot(main_lps) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                            position = "dodge", stat = "identity") + 
  ggtitle("Biomarker Expression with LPS");

#general graph biomarker expression when exposed to TNFalhpa
main_TNFalpha <- main2 %>% filter(cytokine == "TNFalpha");
ggplot(main_TNFalpha) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                            position = "dodge", stat = "identity") + 
  ggtitle("Biomarker Expression with TNFalpha");

#general graph biomarker expression when exposed to IFNnu
main_IFNnu <- main2 %>% filter(cytokine == "IFNnu");
ggplot(main_IFNnu) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                            position = "dodge", stat = "identity") + 
  ggtitle("Biomarker Expression with IFNnu");

#general graph biomarker expression when exposed to IL-1Beta
main_IL1Beta <- main2 %>% filter(cytokine == "IL-1Beta");
ggplot(main_IL1beta) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                            position = "dodge", stat = "identity") + 
  ggtitle("Biomarker Expression with IL-1Beta");

#general graph biomarker expression when exposed to IL-4
main_IL4 <- main2 %>% filter(cytokine == "IL-4");
ggplot(main_IL4) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                            position = "dodge", stat = "identity") + 
  ggtitle("Biomarker Expression with IL-4");

#general graph biomarker expression when exposed to IL-10
main_IL10 <- main2 %>% filter(cytokine == "IL-10");
ggplot(main_IL10) + geom_bar(aes(x = cytokine, y = quantification, fill = Marker), 
                            position = "dodge", stat = "identity") + 
  ggtitle("Biomarker Expression with IL-10");

#heat map of cytokines and biomarkers
ggplot(main2) + geom_tile(mapping = aes(x = cytokine, y = Marker, fill = quantification)) + 
  ggtitle("Cytokine and Correlated Biomarkers");

#*******************************************************************************

#find markers that are highly expressed with LPS
LPS_allMarkers <- main2 %>% filter(cytokine == "LPS");
LPS_allMarkers_mean <- LPS_allMarkers$quantification %>% mean();
LPS_allMarkers %>% filter(quantification > LPS_allMarkers_mean) %>% 
  ggplot() + geom_bar(mapping = aes(x = Marker, y = quantification, fill = Molecule), stat = "identity") +
  ggtitle("Highly Expressed Biomarkers with LPS");

#find which cytokine causes the greatest expression of a specific biomarker
main2 %>% filter(Marker == "iNOS") %>% ggplot() + geom_point(mapping = aes(x = cytokine,
                                                                           y = quantification)) +
  ggtitle("iNOS Expression for each Cytokine");

end