### ===============================================================================================================
### An analysis of university faculty perceptions and practices of using Wikipedia as a teaching resource. 
### ===============================================================================================================

### Objectives
# 1. Display basic statistics about participating teachers, as a ratio of men and women, through graphs and tables.
# 2. Analyze the responses from different user groups for the Perceived Enjoyment category items: ENJ1 and ENJ2.
# 3. Perform other interesting analyzes that present relevant information about the data.

options( scipen = 999, OutDec = "," )

### Step 1 : librarys =============================================================================================

rp <- c("dplyr", "tidyr", "tidyverse", "xtable", "data.table", "formattable", "ggplot2")
lapply(rp, library, character.only = TRUE) # load the required packages

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

str( data ) # here we find out that some variables has the category "?", which we will consider this category as NA ( data2 )
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


### Step 4: Objective 1 ===========================================================================================
# 1. Display basic statistics about participating teachers, as a ratio of men and women, through graphs and tables.

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

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF")

ggplot(gender_pie, aes(x = "", y = freq, fill = GENDER)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  labs(fill = "Gênero") + 
  coord_polar("y", start = 0) +
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 7) +
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
  theme_classic()

  
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
# we consider the 6º domain as "others"
levels( data2$DOMAIN ) <- c( "Artes & Ciências Humanas", 'Ciências', 'Ciências da Saúde', 'Engenharia & Arquitetura', 'Política e Direito', "Outras"  ) # add labels

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

mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF", "#0073C2FF", "#EFC000FF")

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
  theme_classic()


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
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 7) +
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
  geom_text( aes(y = lab.ypos, label = paste0(freq,"%") ), color = "white", size = 7) +
  scale_fill_manual(values = mycols) +
  theme_void() +
  theme(legend.title = element_text(size = 15),
        legend.text = element_text(size = 15))

# # table
names(userwiki) <- c( "Usa Wikipedia?", "Freq. Absoluta", "Freq. Relativa" ) # modifying table's names

formattable( userwiki, 
             align = c("l", rep("r", NCOL(userwiki) - 1)), 
             list( `Freq. Relativa` = percent ) )  # table of frequency and relative frequency to userwiki 



### Step 5: Objective 2 ===========================================================================================

### Step 6: Objective 3 ===========================================================================================

