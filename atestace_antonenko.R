library(tidyverse)
library(scales)
library(naniar)


cvvm = read.csv("https://raw.githubusercontent.com/alesvomacka/Uvod_do_analyzy_dat_v_R/master/data/cvvm_cerven_2019.csv")

ukol = select(.data= cvvm, contains("PO_1"))
ukol = ukol %>%  replace_with_na_all(condition = ~.x =="NEVÍ")

  
ukol2 = data.frame(lapply(ukol, function(x) 
  gsub("naprosto zásadní hrozba", 10,
       gsub("naprosto žádná hrozba", 0,
       gsub("jsme výbornì pøipraveni", 10, 
       gsub("nejsme vùbec pøipraveni", 0, x)))))) 

ukol3 = ukol2 %>% mutate_if(is.character,as.numeric)

hrozby =ukol3 %>% select(PO_109A:PO_111I)
pripravenost = ukol3 %>% select(PO_110A:PO_112I)

colnames(hrozby) = c("Pøírodní katastrofy", 
                         #2 
                         "Epidemie",
                         #3
                         "Dlouhodobé výkyvy poèasí",
                         #4
                         "Únik nebezpeèných chemických/ radioaktivních látek do prostøedí", 
                         #5
                         "Nedostatek potravin a pitné vody",
                         #6
                         "Výpadek elektrické energie",
                         #7
                         "Nedostatek ropy èi plynu",
                         #8
                         "Výpadek internetu, mobilních sítí nebo telefonu",
                         #9
                         "Kybernetický útok",
                         #10
                         "Teroristický útok",
                         #11
                         "Váleèný konflikt",
                         #12
                         "Rabování a výtrnosti",
                         #13
                         "Masová migrace",
                         #14
                         "Stárnutí populace",
                         #15
                         "Nárùst chudoby",
                         #16
                         "Krach bankovního sektoru",
                         #17
                         "Prohlubování ekonomických rozdílù",
                         #18
                         "íøení dezinformací po internetu",
                         #19
                         "Manipulace ve veøejnoprávních médiích",
                         #20
                         "Manipulace v soukromých médiích", 
                         #21
                         "Prohlubování názorových rozdílù",
                         #22
                         "Uchvácení státní moci",
                         #23
                         "Úèast extremistických politických stran ve vládì",
                         #24
                         "Úèast politických stran prosazujících zájmy nepøátelského státu ve vládì",
                         #25
                         "Hospodáøská a energetická závislost na nepøátelském státu",
                         #26
                         "Technologická závislost státu na nadnárodních spoleènostech")
colnames(pripravenost) = c("P_Pøírodní katastrofy", 
                               #2 
                               "P_Epidemie",
                               #3
                               "P_Dlouhodobé výkyvy poèasí",
                               #4
                               "P_Únik nebezpeèných chemických/ radioaktivních látek do prostøedí", 
                               #5
                               "P_Nedostatek potravin/ pitné vody",
                               #6
                               "P_Výpadek elektrické energie",
                               #7
                               "P_Nedostatek ropy èi plynu",
                               #8
                               "P_Výpadek internetu, mobilních sítí nebo telefonu",
                               #9
                               "P_Kybernetický, poèítaèový útok",
                               #10
                               "P_Teroristický útok",
                               #11
                               "P_Váleèný konflikt",
                               #12
                               "P_Rabování a výtrnosti",
                               #13
                               "P_Masová migrace",
                               #14
                               "P_Stárnutí populace",
                               #15
                               "P_Nárust chudoby",
                               #16
                               "P_Krach bankovního sektoru",
                               #17
                               "P_Prohlubování ekonomických rozdílù",
                               #18
                               "P_íøení dezinformací po internetu",
                               #19
                               "P_Manipulace s informacemi ve veøejnoprávních médiích",
                               #20
                               "P_Manipulace s informacemi v soukromých médiích", 
                               #21
                               "P_Prohlubování názorových rozdílù",
                               #22
                               "P_Uchvácení státní moci",
                               #23
                               "P_Úèast extrémistických politických stran ve vládì",
                               #24
                               "P_Úèast politických stran prosazujících zájmy nepøátelského státu ve vládì",
                               #25
                               "P_Hospodáøská/energetická závislost na nepøátelském státu",
                               #26
                               "P_Technologická závislost státu na nadnárodních spoleènostech")

hrozby = summarise_all(hrozby,
                           mean,
                           round(.2),
                           na.rm = TRUE) %>% select_all() %>% pivot_longer(cols = everything())


pripravenost = summarise_all(pripravenost,
                                mean,
                                round(.2),
                                na.rm = TRUE) %>% select_all() %>% pivot_longer(cols = everything())

ukol4 = cbind(hrozby, pripravenost)
colnames(ukol4)  = c("Hrozby", "hrozby_mean", "Pripravenost", "pripravenost_mean")

ukol4 = ukol4 %>% mutate(typ_hrozby = case_when(Pripravenost %in% c("P_Epidemie", 
                                                                       "P_Pøírodní katastrofy",
                                                                       "P_Únik nebezpeèných chemických/ radioaktivních látek do prostøedí",
                                                                       "P_Dlouhodobé výkyvy poèasí",
                                                                       "P_Nedostatek potravin/ pitné vody") ~ "Pøírodní hrozby",
                                                           
                                                           Pripravenost %in% c("P_Výpadek elektrické energie",
                                                                       "P_Výpadek internetu, mobilních sítí nebo telefonu",
                                                                       "P_Nedostatek ropy èi plynu",
                                                                       "P_Kybernetický, poèítaèový útok") ~ "Infrastrukturní hrozby",
                                                           
                                                Pripravenost %in% c("P_Teroristický útok",
                                                                       "P_Masová migrace",
                                                                       "P_Váleèný konflikt",
                                                                       "P_Rabování a výtrnosti") ~ "Konfliktní hrozby",
                                                           
                                                Pripravenost %in% c("P_íøení dezinformací po internetu",
                                                                       "P_Prohlubování názorových rozdílù",
                                                                       "P_Manipulace s informacemi ve veøejnoprávních médiích",
                                                                       "P_Manipulace s informacemi v soukromých médiích") ~ "Informaèní hrozby",
                                                           
                                                Pripravenost %in% c("P_Úèast extrémistických politických stran ve vládì",
                                                                       "P_Úèast politických stran prosazujících zájmy nepøátelského státu ve vládì",
                                                                       "P_Hospodáøská/energetická závislost na nepøátelském státu",
                                                                       "P_Uchvácení státní moci",
                                                                       "P_Technologická závislost státu na nadnárodních spoleènostech") ~"Politické hrozby",
                                                           
                                                Pripravenost %in% c("P_Nárust chudoby",
                                                                       "P_Prohlubování ekonomických rozdílù",
                                                                       "P_Stárnutí populace",
                                                                       "P_Krach bankovního sektoru") ~ "Sociální hrozby"))

ukol4$typ_hrozby = factor(ukol4$typ_hrozby, levels = c("Pøírodní hrozby", "Infrastrukturní hrozby", "Konfliktní hrozby","Informaèní hrozby", "Politické hrozby","Sociální hrozby"))

ukol4 %>%
  ggplot(aes(x = hrozby_mean, y= pripravenost_mean, color = typ_hrozby, label = Hrozby))+
  geom_point(size= 4)+
  scale_color_manual(values = c("#548235", "#ED7D31", "#A5A5A5", "#FFB100", "#4472C4", "#7F6000"))+
  theme_minimal()+
  geom_text(check_overlap = TRUE,hjust = 0, nudge_x = 0.05, size = 3, color = "black")+
  scale_y_continuous(breaks = seq(from = 3.8, to = 4.9, by = 0.1))+
  scale_x_continuous(breaks = seq(from = 4, to = 7, by = 0.5))+
  theme(legend.position = "bottom", legend.title = element_blank())+
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  labs(x ="Míra vnímaného ohrožení",
     y = "Míra vnímané připravenosti",
     title = "Graf 1: Vztah míry vnímaného ohrožení a připravenosti na vybrané hrozby (průměrné hodnoty na škále 0 – 10)",
     caption = "Pozn.: Přerušovaná čára značí přechod, nad kterým je míra vnímané připravenost vyšší, než míra vnímaného ohrožení.
     Pozn.2: Znění položek je pro účely grafu kráceno.
     Zdroj: CVVM SOÚ AV ČR, Naše společnost, 8. – 17. 6. 2019, 1024 respondentů starších 15 let, osobní rozhovor.")
