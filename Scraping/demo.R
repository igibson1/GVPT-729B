#-------------------------------
library(tidyverse)  # data wrangling
library(RSelenium)  # activate Selenium server
library(rvest)      # web scrape tables
library(netstat)    # find unused port
library(data.table) # for the rbindlist function
library(tidyr)
library("stringr")

eCaps <- list(chromeOptions = list(args = c('--disable-gpu', '--lang=en'), 
                                   prefs = list(
                                     "profile.managed_default_content_settings.images" = 2L
                                   )
))

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "129.0.6668.58",
                             verbose = F,
                             port = free_port(),
                             extraCapabilities = eCaps)

remDr <- rs_driver_object$client
remDr$open()
remDr$setTimeout(type = "page load",
                 milliseconds = 200000)
remDr$setTimeout(type = "implicit",
                 milliseconds = 200000)
remDr$maxWindowSize()
remDr$navigate("https://infopemilu.kpu.go.id/Pemilu/Dct_dpd")
Sys.sleep(10)


# setup directories and files
dir.create('C:/demo_id_data')
setwd('C:/demo_id_data')
logs_filename <- 'C:/demo_id_data/logs.txt'


## show 100 items in each region
#dropdownmenu1 <- remDr$findElement(using = "name", value = "tbl_ms_nasional_length")
#dropdownmenu1$clickElement()
#Sys.sleep(2)
dropdown_100_items <- remDr$findElement(using = "css", "option[value='100']")
dropdown_100_items$clickElement()
Sys.sleep(5)

regions_dropdown <- remDr$findElement(using = "id", value = "filterDapil")
regions <- regions_dropdown$selectTag()$text


start_region <- 11 # valid region number 11-96
start_candidate <- 2


#setup regional dataframes
candidate_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c('serial_number', 'full_name', 'gender', 'place_to_stay', 'disability_description', 'img_src', 'region_name', 'motivation'))
employment_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'emp_institution_name', 'emp_department', 'emp_start_year', 'emp_finish_year'))
education_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'edu_level', 'edu_institution_name', 'edu_start_year', 'edu_finish_year'))
organization_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'org_name', 'org_department', 'org_start_year', 'org_finish_year'))
course_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'course_name', 'course_institution_name', 'course_start_year', 'course_finish_year'))
award_df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('full_name', 'award_name', 'awarding_institution_name', 'award_year'))

# setup folders
region_name <- regions[start_region-9]
#print(regions)
region_folder <- sprintf('C:/demo_id_data/%s_%s', start_region, region_name)
dir.create(region_folder)
setwd(region_folder)
#print(getwd())

## select region
selector <- sprintf("option[value='%s']", start_region)

dropdownmenu2 <- remDr$findElement(using = "id", value = "filterDapil")
Sys.sleep(1)
dropdownmenu2$clickElement()
item2 <- remDr$findElement(using = "css", selector)
item2$clickElement()
Sys.sleep(8)


profileButtons <- remDr$findElements(using = "css", "input[class='btn btn-secondary']")
len <- length(profileButtons)
start_region_log_msg <- sprintf("Start Region %s: %s has %s active profiles", start_region-10, region_name, len)
print(start_region_log_msg)
write(start_region_log_msg, file=logs_filename, append=TRUE)

# Extract candidate info
print(sprintf("** Start Region: %s, Candidate: %s", region_name, start_candidate))
tmpProfileButtons <- remDr$findElements(using = "css", "input[class='btn btn-secondary']")
#Sys.sleep(2)
btn <- tmpProfileButtons[[start_candidate]]

#print('-- Scrolling')
loc <- btn$getElementLocation()
script <- sprintf("window.scrollTo(%s,%s);", loc$x, loc$y-500)
remDr$executeScript(script, args = list("dummy"))
#print('-- Scrolled')
Sys.sleep(2)

btn$highlightElement()
btn$clickElement()
#print(cat("btn clicked"))

### extract profile info
Sys.sleep(10)
data_table <- remDr$findElement(using = 'class', 'table')
#print(data_table)
data_table_html <- data_table$getPageSource()
print(data_table_html)
page <- read_html(data_table_html %>% unlist())
df <- html_table(page)
print(df)

# extract photo url
img <- remDr$findElement(using = "css", "img[alt='Foto']") # alt text is Foto or Photo
img_url <- img$getElementAttribute("src")
print(img_url)

### retrieve candidate info
candidate_info <- as.data.frame(matrix(unlist(df[1]), nrow=length(df[1]),byrow=TRUE)) %>%
  select(V2, V3)
serial_number = as.numeric(strsplit(candidate_info[[1]], split = " +")[[1]])[3]

tmp_candidate_info <- data.frame(do.call(rbind, str_split(candidate_info$V3, "\n")), stringsAsFactors = F)
tmp_candidate_info <- data.frame(lapply(tmp_candidate_info, function(x) trimws(x)))
tmp_candidate_info = separate(tmp_candidate_info, X1, into = c("X1", "full_name"), sep = " : ")
tmp_candidate_info = separate(tmp_candidate_info, X2, into = c("X2", "gender"), sep = " :")
tmp_candidate_info = separate(tmp_candidate_info, X3, into = c("X3", "place_to_stay"), sep = " : ")
tmp_candidate_info = separate(tmp_candidate_info, X5, into = c("X5", "disability_description"), sep = ": ")
tmp_candidate_info = tmp_candidate_info %>%
  select(full_name, gender, place_to_stay, disability_description)

## extract motivation df[2]
tmp_candidate_info$motivation = NA
motivation_data <- as.data.frame(df[2])
if (dim(motivation_data)[1] != 0){
  tmp_candidate_info$motivation <- c(motivation_data)
}

#add serial number, image source, region_name to the dataframe
tmp_candidate_info$serial_number <- c(serial_number)
tmp_candidate_info$img_src <- c(img_url)
tmp_candidate_info$region_name <- c(region_name)

candidate_df <- rbind(candidate_df, tmp_candidate_info)
#view(candidate_df)
### retrieve candidate info)


### start: extract employment info
tmp_employment_df <- as.data.frame(df[3])
tmp_employment_df <- tmp_employment_df %>% 
  rename(
    institution_name = NAMA.INSTANSI,
    department = JABATAN,
    emp_start_year = TAHUN.MULAI,
    emp_finish_year = TAHUN.SELESAI,
  )

tmp_employment_df <- dplyr::mutate(tmp_employment_df, full_name = tmp_candidate_info$full_name)
#view(tmp_employment_df)
employment_df <- rbind(employment_df, tmp_employment_df)
#view(employment_df)
### end: extract employment info


### start: extract education info
tmp_edu_df <- as.data.frame(df[4])
tmp_edu_df <- tmp_edu_df %>% 
  rename(
    edu_institution_name = JENJANG.PENDIDIKAN,
    edu_department = NAMA.INSTITUSI,
    edu_start_year = TAHUN.MULAI,
    edu_finish_year = TAHUN.SELESAI,
  )

tmp_edu_df <- dplyr::mutate(tmp_edu_df, full_name = tmp_candidate_info$full_name)
#view(tmp_edu_df)
education_df <- rbind(education_df, tmp_edu_df)
#view(education_df)
### end: extract education info


### start: extract organization info
tmp_org_df <- as.data.frame(df[5])
tmp_org_df <- tmp_org_df %>% 
  rename(
    org_institution_name = NAMA.ORGANISASI,
    org_department = JABATAN,
    org_start_year = TAHUN.MULAI,
    org_finish_year = TAHUN.SELESAI,
  )


tmp_org_df <- dplyr::mutate(tmp_org_df, full_name = tmp_candidate_info$full_name)
#view(tmp_org_df)
organization_df <- rbind(organization_df, tmp_org_df)
#view(organization_df)
### end: extract organization info


### start: extract course and training info
tmp_course_df <- as.data.frame(df[6])
tmp_course_df <- tmp_course_df %>%
  rename(
    course_name = NAMA.KURSUS,
    course_institution_name = LEMBAGA.PENYELENGGARA,
    course_start_year = TAHUN.MULAI,
    course_finish_year = TAHUN.SELESAI,
  )

tmp_course_df <- dplyr::mutate(tmp_course_df, full_name = tmp_candidate_info$full_name)
#view(tmp_course_df)
course_df <- rbind(course_df, tmp_course_df)
#view(course_df)
### end: extract course and training info


### start: extract award info
tmp_award_df <- as.data.frame(df[7])
tmp_award_df <- tmp_award_df %>% 
  rename(
    award_name = NAMA.PENGHARGAAN,
    awarding_institution_name = LEMBAGA.PEMBERI.PENGHARGAAN,
    award_year = TAHUN.MULAI,
  )

tmp_award_df <- dplyr::mutate(tmp_award_df, full_name = tmp_candidate_info$full_name)
#view(tmp_award_df)
award_df <- rbind(award_df, tmp_award_df)
#view(award_df)
### end: extract award info


# navigate to the home page
remDr$goBack()
Sys.sleep(8)
print(sprintf("** End Region: %s, Candidate: %s", region_name, start_candidate))

# save regional dataframes to files
save(candidate_df, file = "candidate.RData")
save(employment_df, file = "employment.RData")
save(education_df, file = "education.RData")
save(organization_df, file = "organization.RData")
save(course_df, file = "course.RData")
save(award_df, file = "award.RData")

end_region_log_msg <- sprintf("End Region %s: %s has %s active profiles", start_region, region_name, len)
print(end_region_log_msg)
write(end_region_log_msg, file=logs_filename, append=TRUE)
  

remDr$close()
rs_driver_object$server$stop()



########If you want to do more than one profile and append at the same time: I am only trying to
#do 3 profiles

#-------------------------------
library(tidyverse)  # data wrangling
library(RSelenium)  # activate Selenium server
library(rvest)      # web scrape tables
library(netstat)    # find unused port
library(data.table) # for the rbindlist function
library(tidyr)
library("stringr")



eCaps <- list(chromeOptions = list(args = c('--disable-gpu', '--lang=en'), 
                                   prefs = list(
                                     "profile.managed_default_content_settings.images" = 2L
                                   )
))

rs_driver_object <- rsDriver(browser = "chrome",
                             chromever = "129.0.6668.58",
                             verbose = F,
                             port = free_port(),
                             extraCapabilities = eCaps)

remDr <- rs_driver_object$client
remDr$open()
remDr$setTimeout(type = "page load",
                 milliseconds = 200000)
remDr$setTimeout(type = "implicit",
                 milliseconds = 200000)
remDr$maxWindowSize()
remDr$navigate("https://infopemilu.kpu.go.id/Pemilu/Dct_dpd")
Sys.sleep(10)


# setup directories and files
dir.create('C:/demo_id_data')
setwd('C:/demo_id_data')
logs_filename <- 'C:/demo_id_data/logs.txt'


## show 100 items in each region
#dropdownmenu1 <- remDr$findElement(using = "name", value = "tbl_ms_nasional_length")
#dropdownmenu1$clickElement()
#Sys.sleep(2)
dropdown_100_items <- remDr$findElement(using = "css", "option[value='100']")
dropdown_100_items$clickElement()
Sys.sleep(5)

regions_dropdown <- remDr$findElement(using = "id", value = "filterDapil")
regions <- regions_dropdown$selectTag()$text


start_region <- 11
start_candidate <- 1

#navigate through each region page, 11-96
for (i in start_region:start_region){
  #setup regional dataframes
  candidate_df <- setNames(data.frame(matrix(ncol = 8, nrow = 0)), c('serial_number', 'full_name', 'gender', 'place_to_stay', 'disability_description', 'img_src', 'region_name', 'motivation'))
  employment_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'emp_institution_name', 'emp_department', 'emp_start_year', 'emp_finish_year'))
  education_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'edu_level', 'edu_institution_name', 'edu_start_year', 'edu_finish_year'))
  organization_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'org_name', 'org_department', 'org_start_year', 'org_finish_year'))
  course_df <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c('full_name', 'course_name', 'course_institution_name', 'course_start_year', 'course_finish_year'))
  award_df <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('full_name', 'award_name', 'awarding_institution_name', 'award_year'))
  
  # setup folders
  region_name <- regions[i-9]
  #print(regions)
  region_folder <- sprintf('C:/demo_id_data/%s_%s', i-10, region_name)
  dir.create(region_folder)
  setwd(region_folder)
  #print(getwd())
  
  ## select region
  selector <- sprintf("option[value='%s']", i)
  
  dropdownmenu2 <- remDr$findElement(using = "id", value = "filterDapil")
  Sys.sleep(1)
  dropdownmenu2$clickElement()
  item2 <- remDr$findElement(using = "css", selector)
  item2$clickElement()
  Sys.sleep(8)
  
  
  profileButtons <- remDr$findElements(using = "css", "input[class='btn btn-secondary']")
  len <- length(profileButtons)
  start_region_log_msg <- sprintf("Start Region %s: %s has %s active profiles", i-10, region_name, len)
  print(start_region_log_msg)
  write(start_region_log_msg, file=logs_filename, append=TRUE)
  
  # Extract candidate info
  for (j in start_candidate:3) {
    print(sprintf("** Start Region: %s, Candidate: %s", region_name, j))
    tmpProfileButtons <- remDr$findElements(using = "css", "input[class='btn btn-secondary']")
    #Sys.sleep(2)
    btn <- tmpProfileButtons[[j]]
    
    #print('-- Scrolling')
    loc <- btn$getElementLocation()
    script <- sprintf("window.scrollTo(%s,%s);", loc$x, loc$y-500)
    remDr$executeScript(script, args = list("dummy"))
    #print('-- Scrolled')
    Sys.sleep(2)
    
    btn$highlightElement()
    btn$clickElement()
    #print(cat("btn clicked"))
    
    ### extract profile info
    Sys.sleep(10)
    data_table <- remDr$findElement(using = 'class', 'table')
    data_table_html <- data_table$getPageSource()
    #print(data_table_html)
    page <- read_html(data_table_html %>% unlist())
    df <- html_table(page)
    #print(df)
    
    # extract photo url
    img <- remDr$findElement(using = "css", "img[alt='Foto']") # alt text is Foto or Photo
    img_url <- img$getElementAttribute("src")
    #print(img_url)
    
    ### retrieve candidate info
    candidate_info <- as.data.frame(matrix(unlist(df[1]), nrow=length(df[1]),byrow=TRUE)) %>%
      select(V2, V3)
    serial_number = as.numeric(strsplit(candidate_info[[1]], split = " +")[[1]])[3]
    
    tmp_candidate_info <- data.frame(do.call(rbind, str_split(candidate_info$V3, "\n")), stringsAsFactors = F)
    tmp_candidate_info <- data.frame(lapply(tmp_candidate_info, function(x) trimws(x)))
    tmp_candidate_info = separate(tmp_candidate_info, X1, into = c("X1", "full_name"), sep = " : ")
    tmp_candidate_info = separate(tmp_candidate_info, X2, into = c("X2", "gender"), sep = " :")
    tmp_candidate_info = separate(tmp_candidate_info, X3, into = c("X3", "place_to_stay"), sep = " : ")
    tmp_candidate_info = separate(tmp_candidate_info, X5, into = c("X5", "disability_description"), sep = ": ")
    tmp_candidate_info = tmp_candidate_info %>%
      select(full_name, gender, place_to_stay, disability_description)
    
    ## extract motivation df[2]
    tmp_candidate_info$motivation = NA
    motivation_data <- as.data.frame(df[2])
    if (dim(motivation_data)[1] != 0){
      tmp_candidate_info$motivation <- c(motivation_data)
    }
    
    #add serial number, image source, region_name to the dataframe
    tmp_candidate_info$serial_number <- c(serial_number)
    tmp_candidate_info$img_src <- c(img_url)
    tmp_candidate_info$region_name <- c(region_name)
    
    candidate_df <- rbind(candidate_df, tmp_candidate_info)
    #view(candidate_df)
    ### retrieve candidate info)
    
    
    ### start: extract employment info
    tmp_employment_df <- as.data.frame(df[3])
    tmp_employment_df <- tmp_employment_df %>% 
      rename(
        institution_name = NAMA.INSTANSI,
        department = JABATAN,
        emp_start_year = TAHUN.MULAI,
        emp_finish_year = TAHUN.SELESAI,
      )
    
    tmp_employment_df <- dplyr::mutate(tmp_employment_df, full_name = tmp_candidate_info$full_name)
    #view(tmp_employment_df)
    employment_df <- rbind(employment_df, tmp_employment_df)
    #view(employment_df)
    ### end: extract employment info
    
    
    ### start: extract education info
    tmp_edu_df <- as.data.frame(df[4])
    tmp_edu_df <- tmp_edu_df %>% 
      rename(
        edu_institution_name = JENJANG.PENDIDIKAN,
        edu_department = NAMA.INSTITUSI,
        edu_start_year = TAHUN.MULAI,
        edu_finish_year = TAHUN.SELESAI,
      )
    
    tmp_edu_df <- dplyr::mutate(tmp_edu_df, full_name = tmp_candidate_info$full_name)
    #view(tmp_edu_df)
    education_df <- rbind(education_df, tmp_edu_df)
    #view(education_df)
    ### end: extract education info
    
    
    ### start: extract organization info
    tmp_org_df <- as.data.frame(df[5])
    tmp_org_df <- tmp_org_df %>% 
      rename(
        org_institution_name = NAMA.ORGANISASI,
        org_department = JABATAN,
        org_start_year = TAHUN.MULAI,
        org_finish_year = TAHUN.SELESAI,
      )
    
    
    tmp_org_df <- dplyr::mutate(tmp_org_df, full_name = tmp_candidate_info$full_name)
    #view(tmp_org_df)
    organization_df <- rbind(organization_df, tmp_org_df)
    #view(organization_df)
    ### end: extract organization info
    
    
    ### start: extract course and training info
    tmp_course_df <- as.data.frame(df[6])
    tmp_course_df <- tmp_course_df %>%
      rename(
        course_name = NAMA.KURSUS,
        course_institution_name = LEMBAGA.PENYELENGGARA,
        course_start_year = TAHUN.MULAI,
        course_finish_year = TAHUN.SELESAI,
      )
    
    tmp_course_df <- dplyr::mutate(tmp_course_df, full_name = tmp_candidate_info$full_name)
    #view(tmp_course_df)
    course_df <- rbind(course_df, tmp_course_df)
    #view(course_df)
    ### end: extract course and training info
    
    
    ### start: extract award info
    tmp_award_df <- as.data.frame(df[7])
    tmp_award_df <- tmp_award_df %>% 
      rename(
        award_name = NAMA.PENGHARGAAN,
        awarding_institution_name = LEMBAGA.PEMBERI.PENGHARGAAN,
        award_year = TAHUN.MULAI,
      )
    
    tmp_award_df <- dplyr::mutate(tmp_award_df, full_name = tmp_candidate_info$full_name)
    #view(tmp_award_df)
    award_df <- rbind(award_df, tmp_award_df)
    #view(award_df)
    ### end: extract award info
    
    
    # navigate to the home page
    remDr$goBack()
    Sys.sleep(8)
    print(sprintf("** End Region: %s, Candidate: %s", region_name, j))
  }
  
  # save regional dataframes to files
  save(candidate_df, file = "candidate.RData")
  save(employment_df, file = "employment.RData")
  save(education_df, file = "education.RData")
  save(organization_df, file = "organization.RData")
  save(course_df, file = "course.RData")
  save(award_df, file = "award.RData")
  
  end_region_log_msg <- sprintf("End Region %s: %s has %s active profiles", i-10, region_name, len)
  print(end_region_log_msg)
  write(end_region_log_msg, file=logs_filename, append=TRUE)
  
}

remDr$close()
rs_driver_object$server$stop()


