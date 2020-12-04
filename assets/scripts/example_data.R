### Create example data for the presentation

#set seed for reproducibility
set.seed(138737)

#adsl
adsl <-  data.frame(SUBJIDN = 1:400,
                    SEX     = sample(c('M','F'), 400, replace = T, prob = c(0.5,0.5)),
                    AGE     = round(runif(400, min=18, max=65)),
                    TRTP    = sample(c('DRUG','PLACEBO'), 400, replace = T, prob=c(0.5,0.5)),
                    WEIGHT  = abs(round(rnorm(400,82, 9),1)),
                    HEIGHT  = abs(round(rnorm(400, 1.75, 0.5),1)),
                    SAFFL   = sample(c('Y','N'),400, replace=TRUE, prob=c(0.8,0.2)),
                    stringsAsFactors = FALSE
)

#mini_adsl
mini_adsl <-  data.frame(SUBJIDN = 1:10,
                         SEX     = sample(c('M','F'), 10, replace = T, prob = c(0.5,0.5)),
                         AGE     = round(runif(10, min=18, max=65)),
                         CNTY    = sample(c("USA","GERMANY","JAPAN","INDIA","CHINA"), 
                                          10, 
                                          replace = T),
                         TRTP    = sample(c('DRUG','PLACEBO'), 10, replace = T, prob = c(0.5,0.5)),
                         TRTSDT  = Sys.Date() + runif(10, min=-365, max=0),
                         SAFFN   = rbinom(10,1,0.8),
                         stringsAsFactors = FALSE
)

mini_adsl <- mini_adsl %>%
              mutate(SEXN = ifelse(SEX=="M",1,0),
                     TRTPN = ifelse(TRTP == "DRUG",1,0)) %>%
              select(SUBJIDN, SEX, SEXN, AGE, CNTY, 
                     TRTP, TRTPN, TRTSDT, SAFFN) %>%
              mutate(CNTY = ifelse(SUBJIDN==10, NA, CNTY))

#Aux Meta Data
aux_meta <- data.frame(var_name = c('AGE','SEX','CNTY'),
                       stringsAsFactors = FALSE)

#adxy
adxy <- data.frame(SUBJIDN = c(2,1,2,3,4,2,1,4,3,4),
                   VISITDT = Sys.Date() + runif(10, min=-175, max=0),
                   AVAL    = round(runif(10, min=130, max=260)))

adxy$VISITDT[3] <- NA

#mini_adsl2
mini_adsl2 <- mini_adsl %>% 
                select(-c(TRTP, TRTPN))

mini_adsl2$SAFFN[2] <- NA

mini_adsl2 <- mini_adsl2 %>%
                mutate(WEIGHT  = abs(round(rnorm(10,82,9),1)),
                       HEIGHT  = abs(round(rnorm(10,1.75,0.5),1)))
             
#mini_adsl3
mini_adsl3  <-  data.frame(SUBJIDN = 1:10,
                           SEX     = rep(c('M','F'), each=5),
                           AGE     = round(runif(10, min=33, max=73)),
                           WEIGHT  = abs(round(rnorm(10,72.23, 17),1)),
                           HEIGHT  = abs(round(rnorm(10, 166, 9),1)),
                           DBP = abs(round(rnorm(10, 73, 9.6),1)),
                           SBP  = abs(round(rnorm(10, 122, 14),1)),
                           stringsAsFactors = FALSE)
#mini_adsl4
mini_adsl4 <- data.frame(SUBJIDN=1:10,
                         QOL_1 = sample(c(1:5), 10, replace=T),
                         QOL_2 = sample(c(1:10), 10, replace=T),
                         QOL_3 = sample(c(1:20), 10, replace=T))

#long_data
long_data <- data.frame(SUBJIDN = c(1,1,1,2,2,3,3,3,4,4),
                        CYCLE =   c("Cycle1","Cycle2","Cycle3","Cycle1","Cycle2","Cycle1","Cycle2","Cycle3","Cycle1","Cycle2"),
                        AVAL    = round(runif(10, min=100, max=200)))

#wide_data
wide_data <- long_data %>% 
              pivot_wider(id_cols = SUBJIDN,
                          names_from = "CYCLE",
                          values_from = "AVAL") %>% as.data.frame()
#fill_data
fill_data <-data.frame(SUBJIDN = c(1,1,1,1,2,2,3,3,3,3),
                       AVAL    = c(10,NA,15,20, NA,12, 30, NA, NA, 15))

#sep_data
sep_data = data.frame(SUBJIDN = 1:4,
                      ASR = c("18/M/NATIVE",
                              "22/F/ASIAN",
                              "65/M/WHITE",
                              "33/F/BLACK"
                      ),
                      stringsAsFactors = FALSE)

#expand + complete data
ec_data <- data.frame(TRTP = c("DRUG","DRUG","DRUG","DRUG","PLACEBO","PLACEBO","PLACEBO","PLACEBO"),
                      AETERM = c("Headache","Dizziness","Headache","Headache",
                                 "Headache","Rash","Headache","Headache"),
                      stringsAsFactors = FALSE)

#simulated bm data!
adbm <- data.frame(subjidn = 1:7,
                   mutation = c('HTSL1 P099X 1.24',
                                'HTSL1 P099X 0.42;HTSL1 P088X 0.11',
                                'JTSL1 F133Y 1.3;HTSL2 P099X 0.504;KTSL3 N888A 4.01',
                                'KTSL3 N888A 4.01',
                                'HTSL1 P088X 0.11;JTSL1 F513Z 7.04',
                                'PPLM2 Z101T 2.64;TTTT0 F513A 2.4',
                                'GSAA1 Z987A 1.23;JTSL3 F311A 12.01'),
                   stringsAsFactors = FALSE)

#excel data
excel_data <- readxl::read_xlsx("assets\\data\\external.xlsx")

#custom ggplot theme
my_theme <- function() {
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14)  %+replace% 
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "top")
}