# Outlier Handling
# Change box plot line colors by groups
p<-ggplot(imputed, aes(x=Transport, y=Age, color=Transport)) +
  geom_boxplot(notch=TRUE,outlier.colour="red", outlier.shape=8,
               outlier.size=4)
p+scale_color_brewer(palette="Dark2")+labs(title="Plot of Salary  as per Transport Usage")

imputed %>%
  select("Age","Work.Exp","Salary","Distance", "Transport") %>% 
  gather(Measure, value, -Transport) %>%                             # Convert to key-value pairs
  ggplot(aes(x = Transport, y = value, color=Transport)) +                     # Plot the values
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)+
  facet_wrap(~ Measure, scales = "free")    # In separate panels




nums <- unlist(lapply(imputed, is.numeric))  
select_if(imputed, is.numeric)
names(dplyr::select_if(imputed,is.numeric))
x  = sapply(imputed, is.numeric)
x<- imputed %>%  keep(is.numeric)
head(x)

# Outlier replacement with na
outlierreplacement <- function(dataframe){
  dataframe %>%          
    map_if(is.numeric, ~ replace(.x, .x %in% boxplot.stats(.x)$out, NA)) %>%
    bind_cols }
outlierreplacement(Clean_Data)
library(dplyr)
# Find and store references to numeric (and integer) variables
numericCols <- imputed %>% summarizeColumns() %>% select(type)
numericCols <- which(numericCols[[1]] %in% c("numeric", "integer"))
numericCols
# Calculate the z scores for each variable and count the number of outliers (values greater than 3)
z_score_outliers <- imputed %>% select(numericCols) %>% mutate_all(.funs = scores, type = "z") %>%
  mutate_all(.funs = function(x) abs(x)>3) %>%
  sapply(sum) %>% as.data.frame() 

# Show the number of outliers per variable
names(z_score_outliers) = "outliers"
z_score_outliers %>% subset(outliers > 0)


# Creater a labeller function to shorten names (just remove the brackets):
shorten_names <-function(x){
  index = str_locate(x, pattern = "\\(") -2 
  out <- substr(x, 1, index)
  out[is.na(out)] <- x[is.na(out)]
  return(out)
}

#Identify which variables contain outliers (according to z-score tests)
outlier.vars <- imputed %>% select(numericCols) %>% mutate_all(.funs = scores, type = "z") %>%
  summarizeColumns() %>% subset(min < -3 | max >3) %>% select(name) %>% unlist()
outlier.vars

#Plot histograms of the data with outliers
imputed %>% select(one_of(outlier.vars)) %>% gather(key = variable, value = value) %>% ggplot(aes(x = value)) + 
  geom_histogram() + facet_wrap(.~variable, scales = "free", labeller = as_labeller(shorten_names))

#Perform the windsoring (function taken from Stack Overflow)
cap <- function(x){
  quantiles <- quantile(x, c(.05,0.25,0.75,.95))
  x[x < quantiles[2] - 1.5*IQR(x)] <- quantiles[1]
  x[x > quantiles[3] + 1.5*IQR(x)] <- quantiles[4]
  return(x)
}

#Perform the capping  
imputed <- imputed %>% mutate_at(.vars =vars("Age", "Work.Exp"  , "Salary", "Distance" ), .funs  = cap)


###################################################################
# For categorical variable
boxplot(Age ~ Salary, data=imputed, main="Transport across age")  # clear pattern is noticeable.
boxplot(Age ~ cut(Salary, pretty(imputed$Salary)), data=imputed, 
        main="Boxplot for Age (categorial) vs Salary", cex.axis=0.5) # this may not be significant, as day of week variable is a subset of the month var.


x <- imputed$Age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
qnt
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
caps
H <- 1.5 * IQR(x, na.rm = T)
H
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
boxplot(x)$out
outval<- boxplot(dat$Age)$out
table(outval)

#find outlier index position in vector
which(dat$Age %in% outval)
dat$Age %in% outval

# Remove outlier from your dataset use the below code.
sales[ !(sales %in%OutVals) ]
dat1<- dat
dat2<- dat1[!(which(dat1$Age %in%outval)),]

dim(dat2)

###################################################
install.packages("rstatix")
library(rstatix)
ls()
list.files()
dat %>% identify_outliers(Age)
imputed %>% identify_outliers(Age)

boxplot(imputed$Age)

############################################################


