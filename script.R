### ===============================================================================================================
### An analysis of university faculty perceptions and practices of using Wikipedia as a teaching resource. 
### ===============================================================================================================

### Objectives
# 1. Display basic statistics about participating teachers, as a ratio of men and women, through graphs and tables.
# 2. Analyze the responses from different user groups for the items of Perceived Enjoyment category: ENJ1 and ENJ2.
# 3. Perform other interesting analyzes that present relevant information about the data.

options( scipen = 999, OutDec = "," )

### Step 1 : librarys =============================================================================================

rp <- c("dplyr", "tidyr", "tidyverse", "xtable", "data.table", "formattable", "ggplot2")
lapply(rp, library, character.only = TRUE) # load the required packages
library(doBy) # the package to use recodeVar function
library(caret)

### Step 2 : import datasets ======================================================================================

data <- fread("wiki4HE.csv")
data2 <- fread("wiki4HE.csv", na.strings = "?")

### Step 3: Data Handling =========================================================================================

### ==========
str( data ) # which class is each variable

# modifying the variables with wrong class
data$GENDER <- as.factor( data$GENDER )
data$DOMAIN <- as.factor( data$DOMAIN )
data$PhD <- as.factor( data$PhD )
data$YEARSEXP <- as.numeric( data$YEARSEXP )
data$UNIVERSITY <- as.factor( data$UNIVERSITY )
data$UOC_POSITION <- as.factor( data$UOC_POSITION )
data$OTHER_POSITION <- as.factor( data$OTHER_POSITION )
data$OTHERSTATUS <- as.factor( data$OTHERSTATUS )
data$USERWIKI <- as.factor( data$USERWIKI )
data[,11:53] <- lapply( data[ , 11:53 ],  factor )

str( data ) # here we find out that some variables has the category "?", which we will consider as NA ( data2 )
# so, we are going to use data2 dataset

### ==========
str( data2 ) 

# modifying the variables with wrong class
data2$GENDER <- as.factor( data2$GENDER )
data2$DOMAIN <- as.factor( data2$DOMAIN )
data2$PhD <- as.factor( data2$PhD )
data2$UNIVERSITY <- as.factor( data2$UNIVERSITY )
data2$UOC_POSITION <- as.factor( data2$UOC_POSITION )
data2$OTHER_POSITION <- as.factor( data2$OTHER_POSITION )
data2$OTHERSTATUS <- as.factor( data2$OTHERSTATUS )
data2$USERWIKI <- as.factor( data2$USERWIKI )
data2[,11:53] <- lapply( data2[ , 11:53 ],  factor )


str( data2 ) # Domain has one level extra; Other = Other_position and other_position = otherstatus; 
             # otherstatus has one level extra

apply( data2, 2, anyNA ) # NA was found in almost all variables (except: AGE, GENDER, PhD, UNIVERSITY)

summary( data2 )

### Step 4: Objective 1 ===========================================================================================
# 1. Display basic statistics about participating teachers, as a ratio of men and women, through graphs and tables.

# UOC_POSITION, OTHER_POSITION and OTHERSTATUS  has a lot of NA's, and we do not consider these as important variables
# so we drop these 3 variables from the analysis
# we also exclude observartions with missing values in DOMAIN, YEARSEXP and USERWIKI variables
# final: 885 obs of 50 variables

data2 <- data2 %>%
  dplyr::select( -c( UOC_POSITION, OTHER_POSITION, OTHERSTATUS ) ) %>%
  drop_na( DOMAIN, YEARSEXP, USERWIKI  )

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#56B4E9", "#009999",  "#E69F00") # setting color palette

### Gender ==========
levels( data2$GENDER ) <- c( "Masculino", "Feminino" ) # add labels

gender <- data2 %>%  
  group_by(GENDER) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to gender

# # pie chart

gender_pie <- gender %>%
  arrange(desc(GENDER)) %>%
  mutate(freq = round(n / sum(n) * 100, 2), lab.ypos = cumsum(freq) - 0.5*freq)
gender_pie 

ggplot(gender_pie, aes(x = "", y = freq, fill = GENDER)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "Gênero") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 10) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# # table
names(gender) <- c( "Gênero", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names
  
formattable( gender, 
             align = c("l", rep("r", NCOL(gender) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to gender 

### AGE ==========

# # boxplot
ggplot(data2, aes(x = "", y = AGE)) + 
  geom_boxplot( fill = mycols[2] ) +
  coord_flip() +
  labs(x = "", y = "Idade (em anos)") +
  theme_classic() +
  theme(text = element_text(size=30) )

# # histogram
ggplot(data2, aes( AGE ) ) + 
  geom_histogram( breaks = seq(20, 70, by = 2),
                  col = mycols[1],
                  bins = sqrt( nrow(data2) ), 
                  fill = mycols[6],
                  alpha = 0.7 ) +
  labs(x = "Idade (em anos)", y = "Frequência") +
  theme_classic() +
  theme(text = element_text(size=30) )

# # table
age <- data2 %>%  
  summarise( "Média" = round( mean(AGE), 2 ), 
             "Mínimo" = min(AGE),
             "1º quartil" = quantile(AGE, probs = 0.25),
             "Mediana" = round( median(AGE), 2 ),
             "2º quartil" = quantile(AGE, probs = 0.75),
             "Máximo" = max(AGE),
             
  )
rownames(age) <- "Idade (em anos)"

formattable(age)

### Domain ==========
# in the dictionary we only have 5 domains, but here we found 6 domains
# we consider the 6º domain as "Social Science, as we found in other dictionary
levels( data2$DOMAIN ) <- c( "Artes & Ciências Humanas", 'Ciências', 'Ciências da Saúde', 'Engenharia & Arquitetura', 'Política e Direito', "Ciências Sociais"  ) # add labels

domain <- data2 %>%  
  drop_na(DOMAIN) %>% # drop only 2 observations
  group_by(DOMAIN) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to domain (whithout NA) 

# # pie chart

domain_pie <- domain %>%
  arrange(desc(DOMAIN)) %>%
  mutate(freq = round(n / sum(n) * 100, 2), lab.ypos = cumsum(freq) - 0.5*freq)
domain_pie 

ggplot(domain_pie, aes(x = "", y = freq, fill = DOMAIN)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "Área do Conhecimento") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 7) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# # table
names(domain) <- c( "Área do conhecimento", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( domain, 
             align = c("l", rep("r", NCOL(domain) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to domain 

### PhD ==========
levels( data2$PhD ) <- c( "Não", "Sim" ) # add labels

phd <- data2 %>%  
  group_by(PhD) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to phd

# # pie chart

phd_pie <- phd %>%
  arrange(desc(PhD)) %>%
  mutate(freq = round(n / sum(n) * 100, 2), lab.ypos = cumsum(freq) - 0.5*freq)
phd_pie 

ggplot(phd_pie, aes(x = "", y = freq, fill = PhD)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "PhD") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 10) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# # table
names(phd) <- c( "PhD", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( phd, 
             align = c("l", rep("r", NCOL(phd) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to phd 

### YEARSEXP ==========

# # boxplot
ggplot(data2, aes(x = "", y = YEARSEXP)) + 
  geom_boxplot( fill = mycols[2] ) +
  coord_flip() +
  labs(x = "", y = "Anos de experiência em ensino universitário ") +
  theme_classic()+
  theme(text = element_text(size=30) )

# # histogram
ggplot(data2, aes( YEARSEXP ) ) + 
  geom_histogram( breaks = seq(0, 44, by = 2),
                  col = mycols[1],
                  bins = sqrt( nrow(data2) ), 
                  fill = mycols[6],
                  alpha = 0.7 ) +
  labs(x = "Anos de experiência em ensino universitário", y = "Frequência") +
  theme_classic() +
  theme(text = element_text(size=30) )

# # table
yearsexp <- data2 %>%  
  drop_na(YEARSEXP) %>%
  summarise( "Média" = round( mean(YEARSEXP), 2 ), 
             "Mínimo" = min(YEARSEXP),
             "1º quartil" = quantile(YEARSEXP, probs = 0.25),
             "Mediana" = round( median(YEARSEXP), 2 ),
             "2º quartil" = quantile(YEARSEXP, probs = 0.75),
             "Máximo" = max(YEARSEXP),
             
  )
rownames(yearsexp) <- 'Anos de experiência em ensino universitário'

formattable(yearsexp)

### University ==========
levels( data2$UNIVERSITY ) <- c( "UOC", "UPF" ) # add labels

university <- data2 %>%  
  group_by(UNIVERSITY) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to university

# # pie chart

university_pie <- university %>%
  arrange(desc(UNIVERSITY)) %>%
  mutate(freq = round(n / sum(n) * 100, 2), lab.ypos = cumsum(freq) - 0.5*freq)
university_pie 

ggplot(university_pie, aes(x = "", y = freq, fill = UNIVERSITY)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "Universidade") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 9) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# # table
names(university) <- c( "Universidade", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( university, 
             align = c("l", rep("r", NCOL(university) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to university 

### Userwiki ==========
levels( data2$USERWIKI ) <- c( "Não", "Sim" ) # add labels

userwiki <- data2 %>% 
  drop_na(USERWIKI) %>% # drop 4 observations
  group_by(USERWIKI) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to userwiki

# # pie chart

userwiki_pie <- userwiki %>%
  arrange(desc(USERWIKI)) %>%
  mutate(freq = round(n / sum(n) * 100, 2), lab.ypos = cumsum(freq) - 0.5*freq)
userwiki_pie 

ggplot(userwiki_pie, aes(x = "", y = freq, fill = USERWIKI)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "Usa Wikipedia?") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 9) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# # table
names(userwiki) <- c( "Usuário Wikipedia?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( userwiki, 
             align = c("l", rep("r", NCOL(userwiki) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to userwiki 



### Step 5: Objective 2 ===========================================================================================
# 2. Analyze the responses from different user groups for the items of Perceived Enjoyment category: ENJ1 and ENJ2.
# Perceived Enjoyment
# ENJ1: The use of Wikipedia stimulates curiosity
# ENJ2: The use of Wikipedia is entertaining

### ENJ1 ===============================================================

levels( data2$ENJ1 ) = c( "Discordo Totalmente", "Discordo Parcialmente",
                          "Não concordo, nem discordo",
                          "Concordo Parcialmente", "Concordo Totalmente")

### ENJ1 by gender ==========

ENJ1_gender <- data2 %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(GENDER, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ1

ggplot( ENJ1_gender, aes(x = GENDER, y = freq, fill = ENJ1) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Gênero" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size=30))

# # table
names(ENJ1_gender) <- c( "Gênero", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ1_gender, 
             align = c("l", rep("r", NCOL(ENJ1_gender) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ1 

### ENJ1 by DOMAIN ==========

ENJ1_DOMAIN <- data2 %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(DOMAIN, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ1

ggplot( ENJ1_DOMAIN, aes(x = DOMAIN, y = freq, fill = ENJ1) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Área do Conhecimento" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 10),
        axis.text.x = element_text(angle=45, hjust=1))

# # table
names(ENJ1_DOMAIN) <- c( "Área do Conhecimento", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ1_DOMAIN, 
             align = c("l", rep("r", NCOL(ENJ1_DOMAIN) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ1 

### ENJ1 by PhD ==========

ENJ1_PhD <- data2 %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(PhD, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ1

ggplot( ENJ1_PhD, aes(x = PhD, y = freq, fill = ENJ1) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Possui PhD?" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# # table
names(ENJ1_PhD) <- c( "Possui Doutorado?", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ1_PhD, 
             align = c("l", rep("r", NCOL(ENJ1_PhD) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ1 

### ENJ1 by UNIVERSITY ==========

ENJ1_UNIVERSITY <- data2 %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(UNIVERSITY, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ1

ggplot( ENJ1_UNIVERSITY, aes(x = UNIVERSITY, y = freq, fill = ENJ1) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Universidade em que leciona" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# # table
names(ENJ1_UNIVERSITY) <- c( "Universidade", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ1_UNIVERSITY, 
             align = c("l", rep("r", NCOL(ENJ1_UNIVERSITY) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ1 

### ENJ1 by USERWIKI ==========

ENJ1_USERWIKI <- data2 %>% 
  drop_na(ENJ1) %>% # drop 7 observations
  group_by(USERWIKI, ENJ1) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ1

ggplot( ENJ1_USERWIKI, aes(x = USERWIKI, y = freq, fill = ENJ1) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "É usuário da Wikipédia?" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# # table
names(ENJ1_USERWIKI) <- c( "É usuário da Wikipédia?", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ1_USERWIKI, 
             align = c("l", rep("r", NCOL(ENJ1_USERWIKI) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ1 

### ENJ2 ===============================================================

levels( data2$ENJ2 ) = c( "Discordo Totalmente", "Discordo Parcialmente",
                          "Não concordo, nem discordo",
                          "Concordo Parcialmente", "Concordo Totalmente")

### ENJ2 by gender ==========

ENJ2_gender <- data2 %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(GENDER, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ2

ggplot( ENJ2_gender, aes(x = GENDER, y = freq, fill = ENJ2) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Gênero" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size=30))

# # table
names(ENJ2_gender) <- c( "Gênero", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ2_gender, 
             align = c("l", rep("r", NCOL(ENJ2_gender) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ2 

### ENJ2 by DOMAIN ==========

ENJ2_DOMAIN <- data2 %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(DOMAIN, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ2

ggplot( ENJ2_DOMAIN, aes(x = DOMAIN, y = freq, fill = ENJ2) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Área do Conhecimento" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 10),
        axis.text.x = element_text(angle=45, hjust=1))

# # table
names(ENJ2_DOMAIN) <- c( "Área do Conhecimento", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ2_DOMAIN, 
             align = c("l", rep("r", NCOL(ENJ2_DOMAIN) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ2 

### ENJ2 by PhD ==========

ENJ2_PhD <- data2 %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(PhD, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ2

ggplot( ENJ2_PhD, aes(x = PhD, y = freq, fill = ENJ2) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Possui PhD?" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# # table
names(ENJ2_PhD) <- c( "Possui Doutorado?", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ2_PhD, 
             align = c("l", rep("r", NCOL(ENJ2_PhD) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ2 

### ENJ2 by UNIVERSITY ==========

ENJ2_UNIVERSITY <- data2 %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(UNIVERSITY, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ2

ggplot( ENJ2_UNIVERSITY, aes(x = UNIVERSITY, y = freq, fill = ENJ2) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "Universidade em que leciona" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# # table
names(ENJ2_UNIVERSITY) <- c( "Universidade", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ2_UNIVERSITY, 
             align = c("l", rep("r", NCOL(ENJ2_UNIVERSITY) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ2 

### ENJ2 by USERWIKI ==========

ENJ2_USERWIKI <- data2 %>% 
  drop_na(ENJ2) %>% # drop 7 observations
  group_by(USERWIKI, ENJ2) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to ENJ2

ggplot( ENJ2_USERWIKI, aes(x = USERWIKI, y = freq, fill = ENJ2) ) +
  geom_bar(stat = "identity", color = "white" ) +
  labs(fill = "É interessante?", y = "Frequência Relativa", x = "É usuário da Wikipédia?" ) + 
  scale_fill_manual(values = mycols) +
  theme_classic() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# # table
names(ENJ2_USERWIKI) <- c( "É usuário da Wikipédia?", "É interessante?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( ENJ2_USERWIKI, 
             align = c("l", rep("r", NCOL(ENJ2_USERWIKI) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to ENJ2 


### Step 6: Objective 3 ===========================================================================================
# 3. Perform other interesting analyzes that present relevant information about the data.

### Score ==========

# We propose a single measure of the agreement with the view that using Wikipedia as a teaching resource is good, for all of our respondents.
# Alternatives: IRT (ITEM RESPONDE THEORY) technique or padronized score.
# we are going to use padronized score (do not remove or imput missing values).

# # Replace NA's with mean of the corresponding item 
# imput_NA <- function(x) replace(x, is.na(x), round(mean(x, na.rm = TRUE)) )
# data_imput <- as.data.frame( apply( apply(data2[,8:50], 2, as.numeric), 2, imput_NA ) )

### calculating score
# max: 100 | min: 0

levels( data2$ENJ1 ) = 1:5
levels( data2$ENJ2 ) = 1:5

score <- as.data.frame( apply( data2[,8:50], 2, as.numeric  ) ) - 1
score$Qu4 <- recodeVar( score$Qu4, c(0,1,2,3,4), c(4,3,2,1,0) ) # recoding QU4, because agree with that question is a "bad thing"

score$escore_total=NULL
score$escore_maximo=NULL
score$escore_padronizado=NULL

cont.na <- NULL
n_var <- 43
for(i in 1:nrow(score)){
  cont.na[i] <- 0
  cont.na[i] <- sum( is.na( score[i,1:n_var] ) )
  score$escore_total[i] <- sum( score[i,1:n_var], na.rm = T)
  score$escore_maximo[i] <- 4 * ( n_var - cont.na[i] )
  score$escore_padronizado[i] <- round( score$escore_total[i] / score$escore_maximo[i] * 100 )
}

summary(score$escore_maximo)
summary(score$escore_padronizado)

# # histogram
ggplot(score, aes( escore_padronizado ) ) + 
  geom_histogram( breaks = seq(0, 100, by = 5),
                  col = mycols[1],
                  bins = sqrt( nrow(data2) ), 
                  fill = mycols[6],
                  alpha = 0.7 ) +
  labs(x = "Score padronizado", y = "Frequência") +
  theme_classic() +
  theme(text = element_text(size=30) )

# # table
escore_padronizado <- score %>%  
  summarise( "Média" = round( mean(escore_padronizado), 2 ), 
             "Mínimo" = min(escore_padronizado),
             "1º quartil" = quantile(escore_padronizado, probs = 0.25),
             "Mediana" = round( median(escore_padronizado), 2 ),
             "2º quartil" = quantile(escore_padronizado, probs = 0.75),
             "Máximo" = max(escore_padronizado),
             
  )
rownames(escore_padronizado) <- 'Score'

formattable(escore_padronizado)

# # recommend (yes or no)
score$recommend = data2$recommend = ifelse( score$escore_padronizado >= 50, "Sim", "Não"  )

recommend <- data2 %>% 
  drop_na(recommend) %>% # drop 4 observations
  group_by(recommend) %>%
  summarise(n = n()) %>%
  mutate( freq = n / sum(n)  )  # table of frequency and relative frequency to recommend

recommend_pie <- recommend %>%
  arrange(desc(recommend)) %>%
  mutate(freq = round(n / sum(n) * 100, 2), lab.ypos = cumsum(freq) - 0.5*freq)
recommend_pie 

ggplot(recommend_pie, aes(x = "", y = freq, fill = recommend)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "Recomenda Wikipedia?") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(n," ( ",freq," % )") ), color = "white", size = 9) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))


### Prediction ==========

data2$recommend <- as.factor( data2$recommend )

# seed
set.seed(666)

# train (70%) e test (30%)
idx = createDataPartition(y = data2$recommend, p = 0.7, list=FALSE)
train = data2[idx, ]
test = data2[-idx, ]

# # # Regressao Logistica ============

mod = glm( recommend ~ AGE + GENDER + DOMAIN + PhD + YEARSEXP + UNIVERSITY + USERWIKI,
           data = train, family = binomial( link = "logit" ) )
summary(mod) # remove YEARSEXP 

mod2 = glm( recommend ~ AGE + GENDER + DOMAIN + PhD + UNIVERSITY + USERWIKI,
           data = train, family = binomial( link = "logit" ) )
summary(mod2) # remove UNIVERSITY 

mod3 = glm( recommend ~ AGE + GENDER + DOMAIN + PhD + USERWIKI,
            data = train, family = binomial( link = "logit" ) )
summary(mod3) # remove GENDER 

mod4 = glm( recommend ~ AGE  + DOMAIN + PhD + USERWIKI,
            data = train, family = binomial( link = "logit" ) )
summary(mod4) 

model = mod4

#predictions
pred <- predict(model, test, type = "response")

# transform prediction into classification
prediction <- as.numeric(pred > 0.5)

result_predict_classico <- data.frame(actual = as.factor( as.numeric(test$recommend) - 1 ),
                                       predict = as.factor(prediction))

head(result_predict_classico)

# Label 1 - Recommend wikipedia
# Label 0 - Do not recommend wikipedia

# Confusion Matrix with Caret
confusionMatrix(result_predict_classico$actual, as.factor(result_predict_classico$predict))
