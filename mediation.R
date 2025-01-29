# https://rpubs.com/mbounthavong/mediation_analysis_r

# To install MEPS package in R, you need to do a couple of things.
### Step 1: Install the "devtools" package. 
install.packages("devtools")
install.packages("gtsummary")
install.packages("expss")
install.packages("bda")
install.packages("mediation")

### Step 2: Install the "MEPS" package from the AHRQ MEPS GitHub site. 
devtools::install_github("e-mitchell/meps_r_pkg/MEPS")

### step 3: Load the MEPS package
library("MEPS") ## You need to load the library every time you restart R

### Step 4: Load the other libraries
library("dplyr")          # Data wrangling
library("gtsummary")      # Create tables
library("expss")          # Relabel variables
library("bda")            # Perform Sobel text
library("mediation")        # Perform the bootstrap approach

# There are two ways to load data from AHRQ MEPS website:
#### Method 1: Load data from AHRQ MEPS website
hc2021 = read_MEPS(file = "h233")
View(hc2021)

#### Method 2: Load data from AHRQ MEPS website
hc2021 = read_MEPS(year = 2021, type = "FYC")

### Step 5: Change column names to lowercase
names(hc2021) <- tolower(names(hc2021))

### Step 6: Select specific variables
### 2021
hc2021p = hc2021 %>%
  rename(
    workdays = ddnwrk21,
    diabetes = diabdx_m18,
    health_status = rthlth31) %>%
  dplyr::select(                        ## NOTE: there is a weird issue when you don't involve dplyr::select bc MASS is preferred (URL: https://stackoverflow.com/questions/48161431/select-statement-error-unused-argument)
    dupersid, 
    workdays, 
    diabetes, 
    health_status,
    sex)
hc2021p$year <- 2021

### Step 7: Clean data (We don't want to include any missing or NA responses)
hc2021p = hc2021p %>%
  filter(workdays >= 0,
         diabetes >= 1,
         health_status >= 1)

# We want "No diabetes" to have a value of 0 because it will make interpreting the model easier
hc2021p$diabetes[hc2021p$diabetes == 2] = 0

### Step 8: Convert to factor and add labels
hc2021p$sex <- factor(hc2021p$sex,
                      levels = c(1, 2),
                      labels = c("Male", "Female"))

hc2021p$health_status <- factor(hc2021p$health_status, 
                                levels = c(1, 2, 3, 4, 5),
                                labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))

hc2021p$diabetes <- factor(hc2021p$diabetes,
                           levels = c(0, 1),
                           labels = c("No diabetes", "Diabetes"))

### Step 9: Relabel the variables
hc2021p <- apply_labels(hc2021p,
                        diabetes = "Diabetes status", 
                        workdays = "Days missed from work", 
                        health_status = "Perceived health status", 
                        sex = "Sex")

View(hc2021p)

## Descriptive Analysis Table
hc2021p %>%
  tbl_summary(include = c(health_status, sex, workdays), 
              by = diabetes, 
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label = "**Variable***") %>%
  bold_labels()

### Regression Model -- Total Effect
direct.model <- glm(workdays ~ diabetes, data = hc2021p, family = gaussian(link = "identity"))
round(cbind(coef(direct.model), confint(direct.model)), 3)

tbl_regression(direct.model, estimate_fun = ~ style_number(.x, digits = 3))

### Regression model - First-part Indirect effect
indirect.model1 <- glm(as.numeric(health_status) ~ diabetes, data = hc2021p, family = gaussian(link = "identity")) ## We need to convert the `health_status` variable to numeric to make sense of the linear regression model.
round(cbind(coef(indirect.model1), confint(indirect.model1)), 3)

tbl_regression(indirect.model1, estimate_fun = ~ style_number(.x, digits = 3))

### Regression model - Second-part Direct + Indirect effects
indirect.model2 <- glm(workdays ~ diabetes + as.numeric(health_status), data = hc2021p, family = gaussian(link = "identity"))
round(cbind(coef(indirect.model2), confint(indirect.model2)), 3)

tbl_regression(indirect.model2, estimate_fun = ~ style_number(.x, digits = 3))

### Compare the Results
t1 <- tbl_regression(direct.model, estimate_fun = ~ style_number(.x, digits = 3)) %>% modify_column_hide(columns = p.value) %>% modify_column_hide(ci)
t2 <- tbl_regression(indirect.model1, estimate_fun = ~ style_number(.x, digits = 3)) %>% modify_column_hide(columns = p.value) %>% modify_column_hide(ci)
t3 <- tbl_regression(indirect.model2, estimate_fun = ~ style_number(.x, digits = 3)) %>% modify_column_hide(columns = p.value) %>% modify_column_hide(ci)

tbl_merge(
  tbls = list(t1, t2, t3),
  tab_spanner = c("**Total effect**", "**First-part indirect effect**", "**Second-part direct + indirect effects**")
)


### Sobel test (Proportion test) -- Mediation
## mediation.test(mv,iv,dv), where mv is the mediator variable, iv is the independent varible, and dv is the dependent variable)
mediation <- mediation.test(as.numeric(hc2021p$health_status), hc2021p$diabetes, hc2021p$workdays)
round(mediation, 3)

## Note: We have to convert health_status to a numeric since errors will occur when using it as a factor.
hc2021p$health_status <- as.numeric(hc2021p$health_status)

## First-part indirect effect model:
indirect.model1 <- glm(health_status ~ diabetes, data = hc2021p, family = gaussian(link = "identity"))

## Second-part direct + indirect effect model:
indirect.model2 <- glm(workdays ~ diabetes + health_status, data = hc2021p, family = gaussian(link = "identity"))

## Mediation analysis with 1000 simulations
mediation.results <- mediate(indirect.model1, indirect.model2, treat = 'diabetes', mediator = 'health_status', boot = TRUE, sims = 1000)
summary(mediation.results)
