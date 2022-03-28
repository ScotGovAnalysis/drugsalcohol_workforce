library(tidyverse)
library(zip)
library(lubridate)
library(stringr)
library(openxlsx)

datashare_backup<-"//scotland.gov.uk/dc1/fs3_home/U449921/workforce"

#Other folders a part from root to backup
# folders_to_backup<-list()
# folders_to_backup<-list("Desktop Instructions/",
#                         "R Code/",
#                         "R Markdown/")

#======================================

dir.create(datashare_backup, showWarnings = FALSE)

#Copy everything in the following directories to the local backup

#Set up the Backup Directory
local_backup<-"Local Backup/"
dir.create(local_backup, showWarnings = FALSE)

#root_folder
backup_myfolder<-local_backup
myfolder<-"."
list_of_files <- list.files(myfolder, pattern=".xlsm|.lnk|.R|.Rproj|.docx") 
file.copy(file.path(myfolder,list_of_files), backup_myfolder)

#=======================================
#Functions
copy_folders_to_backup <- function(myfolder) {
  backup_myfolder <- paste0(local_backup, myfolder)
  dir.create(backup_myfolder, showWarnings = FALSE)
  list_of_files <- list.files(myfolder, "*.*") 
  file.copy(file.path(myfolder,list_of_files), backup_myfolder, overwrite = TRUE) 
}

split_path <- function(x) { if (dirname(x)==x) x else c(basename(x),split_path(dirname(x)))}

#End of Functions
#================================================

lapply(folders_to_backup, copy_folders_to_backup)

#================================================
#Create a BackUp Version of the  Data
#Corporate Backups on Datashare and versions saved to eRDM.
#This will be last three months worth (last three versions)

#================================================

wd_location<-getwd()
where_am_i<-split_path(wd_location)
type_name<-str_extract_all(where_am_i[1], boundary("word"))
type_name<-type_name[[1]][1]

#Backup File Name
my_back_upfile<-paste0(type_name, " Data Processing Backup ",now(),".zip")
my_back_upfile<-str_replace_all(my_back_upfile,":","")

##Put in the full folder name
zip_this_folder<-paste0(wd_location, "/", "Local Backup")

zip::zipr(zipfile=my_back_upfile,
          files=zip_this_folder,
          recurse = TRUE,
           compression_level = 9)
 
# Copy to the Datashare
  file.copy(my_back_upfile,datashare_backup)
  
#  Remove Zip File
  file.remove(my_back_upfile)
  
#  Remove Local Backup Folder Contents
  local_backup<-str_replace_all(local_backup,"/","")
  unlink(local_backup, recursive = TRUE)
  
#Recreate Backup Dir otherwise it upsets project  
  dir.create(local_backup)
  
#  ============================================================
 # Tidy Up the Back Up Folder
  
  df <- file.info(list.files(datashare_backup, full.names = T))
#  Calculte the age of the files
  df<-df %>% tibble::rownames_to_column("myfilename") %>%
    mutate(t0=ymd_hms(mtime),
                    t1=now(),
                    age=t0 %--% t1,
                    tdiff=as.duration(age) /dweeks(1)) %>% 
 #   Only keep five most recent
    arrange(age) %>%
    rowid_to_column("rowid") 
  
  delete_me <-df %>% filter(grepl(type_name, myfilename) & rowid > 10)
  
  for (i in 1:nrow(delete_me)) {
    
    delete_filename<-delete_me$myfilename[i]
    unlink(delete_filename)
    }
