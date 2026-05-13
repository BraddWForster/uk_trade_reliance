##    Programme:  R
##
##    Objective: Look at the UK's import reliance on China.
##
##    Plan of  : 
##    Attack   :  
##
##               1. Download UK imports via COMTRADE API.
##               2. Summarise the import data.
##               3. Collect UK imports from China at HS2 level.
##               4. Collect China's exports to the world.
##               5. Join data.
##               6. Plot the risk.
##
##
##    Author   :  Bradd Forster
##
##  Clear the decks and load up some functionality
    ##
    rm(list=ls(all=TRUE))

##  Core libraries
    ##
    library(dplyr)
    library(tidyr)
    library(readxl)
    library(writexl)
    library(stringr)
    library(lubridate)
    library(ggplot2)

##  Optional libraries
    ##
    # install.packages("comtradr")
    library(comtradr)
    
##  Set up paths for working directories
    ##
    userid <- "[YOUR NAME]"
    r_d <- paste0("[YOUR DIRECTORY]")
    output_d <- paste0("[YOUR DIRECTORY]")
      
##  Setting the working directory
    ##
    setwd(r_d)

##  Set API key
    ##
    Sys.setenv('COMTRADE_PRIMARY' = '[YOU NEED YOUR OWN ONE :) ]')  # you need your own COMTRADE API key

##  Style for website graphs
    ##
    theme_mywebsite <- function(base_size = 9, base_family = "Montserrat") {
      theme_minimal(base_size = base_size, base_family = base_family) +
        theme(
          panel.grid.major = element_line(color = "#5A636A", size = 0.2),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = base_size + 2, face = "bold", color = "#AFB3B7"),
          axis.text = element_text(size = base_size, color = "#AFB3B7"),
          legend.title = element_text(size = base_size + 1, face = "bold", color = "#AFB3B7"),
          legend.text = element_text(size = base_size, color = "#AFB3B7"),
          plot.title = element_text(size = base_size + 4, face = "bold", hjust = 0.5, color = "#AFB3B7"),
          plot.subtitle = element_text(size = base_size + 1, color = "#AFB3B7", hjust = 0.5),
          plot.background = element_rect(fill = "#132E35", color = "#132E35"),
          legend.position = "bottom",
          legend.background = element_blank(),
          legend.key = element_blank()
        )
    }
        
################################################################################
## 1. Download UK imports via COMTRADE API.
################################################################################
    
# View(country_codes)  # variables in the COMTRADE API
# ?ct_get_data()  # documentation
    
df_list <- list()  # create a list to store annual data

for (i in 2023:2025) {
  print(i)
  
  df <- comtradr::ct_get_data(  # function for using API to call trade data from COMTRADE
        type = "goods",
        frequency = "A",
        reporter = "GBR",  # get the UK's imports
        partner = "all_countries",
        commodity_classification = "HS",
        commodity_code = "TOTAL",  # get total imports
        start_date = i,  # collect the one years worth of data
        end_date = i, 
        flow_direction = "Import",
      )
  
  df_list[[paste0(i)]] <- df
  
}

df <- do.call(rbind, df_list)  # create a single data frame

raw_m <- df  # storing raw results


################################################################################
##  2. Summarise the import data.
################################################################################

df <- aggregate(primary_value ~ reporter_iso + flow_code + partner_iso + partner_desc + cmd_code, data = df, FUN = mean, na.rm = TRUE)  # 3-year average

df <- df[order(desc(df$primary_value)),]  # order descending in 3-year average trade value

# sort(unique(df$partner_desc))  # look at the countries that the UK imports from

df[df$partner_desc == "United Kingdom","primary_value"]/sum(df$primary_value)  # beware re-imports (1.7%)

gbr_m <- sum(df$primary_value)  # total 3-year average UK imports

df$Share <- df$primary_value / gbr_m

df <- df[order(desc(df$Share)),]

# remove the catch alls

df <- df[df$partner_iso != "_X ",]  # includes bunkers, western sahara, kosovo etc.
df <- df[df$partner_iso != "E19",]  # Europe but not specified 
df$partner_iso <- ifelse(df$partner_iso == "S19", "TWN", df$partner_iso)  # Taiwan


p1 <- ggplot(data = df[1:30,]) +  # plot
                  geom_col(aes(x = reorder(partner_iso, -Share), y = Share), fill = "orange") +
                  scale_y_continuous(labels = scales::percent) +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
                  labs(x = "Country", title = "Share of UK imports (2023-2025 average)")

p1 <- p1 +
                  theme_mywebsite() +
                  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

p1

setwd(output_d)

png("uk_imports_by_origin_v1.png",  width = 2000, height = 1600, res = 300)
print(p1)
dev.off()

## Let's proceed with China.


################################################################################
##  3. Collect UK imports from China at HS2 level.
################################################################################

ref <- comtradr::ct_get_ref_table('HS')  # inspect HS codes

ref <- ref[ref$aggrLevel == 2,]  # HS2 

hs2 <- ref$id  # store HS2 codes as a vector


df_list <- list()  # create a list to store annual data

for (i in 2023:2025) {
  print(i)
  
  df <- comtradr::ct_get_data(  # function for using API to call trade data from COMTRADE
    type = "goods",
    frequency = "A",
    reporter = "GBR",  # get the UK's imports
    partner = c("CHN", "World"),
    commodity_classification = "HS",
    commodity_code = hs2,  # get HS2 imports
    start_date = i,  # collect the three years worth of data
    end_date = i, 
    flow_direction = "Import",
  )
  
  df_list[[paste0(i)]] <- df
  
}

df <- do.call(rbind, df_list)

raw_gbr_m <- df  # store

df <- aggregate(primary_value ~ reporter_iso + flow_code + partner_iso + partner_desc + cmd_code + cmd_desc, data = raw_gbr_m, FUN = mean, na.rm = TRUE)  # taking the mean

df <- reshape(df,  # making the long data wide to calculate the share
              idvar = c("reporter_iso", "flow_code", "cmd_code", "cmd_desc"),
              timevar = "partner_iso",
              v.names = "primary_value",
              direction = "wide",
              drop = "partner_desc"
              )

df$ShareUkImports <- df$primary_value.CHN / df$primary_value.W00  # calculate the share of UK's imports from China by HS2

y_axis <- df


################################################################################
##  4. Collect China's exports to the world.
################################################################################

df_list <- list()

for (i in hs2) {
  print(i)
  
  df <- comtradr::ct_get_data(  # function for using API to call trade data from COMTRADE
    type = "goods",
    frequency = "A",
    reporter = "all_countries",  # get the every countries exports
    partner = "World",  # to UK and the rest of the world
    commodity_classification = "HS",
    commodity_code = i,  # get HS2 imports
    start_date = 2023,  # collect the three years worth of data
    end_date = 2025, 
    flow_direction = "Export",
  )
  
  df_list[[paste0(i)]] <- df
  
}

expected_cols <- max(sapply(df_list, ncol))  # sometimes there is no data and this will create a problem with rbind()
df_list <- df_list[sapply(df_list, ncol) == expected_cols]
df <- do.call(rbind, df_list)

raw_x <- df  # store every countries exports to UK and World

# unique(raw_x[,c("reporter_iso", "reporter_desc")])  # inspect countries

df <- aggregate(primary_value ~ reporter_iso + flow_code + partner_iso + partner_desc + cmd_code + cmd_desc, data = df, FUN = mean, na.rm = TRUE)  # taking the mean

df_chn <- df[df$reporter_iso == "CHN",]  # filtering for China

df <- aggregate(primary_value ~ flow_code + partner_iso + partner_desc + cmd_code + cmd_desc, data = df, FUN = sum, na.rm = TRUE)  # taking the total exports

df <- df[,-(1:3)]
colnames(df) <- c("cmd_code", "cmd_desc", "WorldExports")

df_chn <- df_chn[,-(1:4)]
colnames(df_chn) <- c("cmd_code", "cmd_desc", "ChinaExports")

df <- merge(df, df_chn, all.x = TRUE, by = c("cmd_code", "cmd_desc"))  # put China and World exports together

df$ChinaExports <- ifelse(is.na(df$ChinaExports),0,df$ChinaExports)  # replace NAs with 0's

df$ShareWorldExports <- df$ChinaExports / df$WorldExports

x_axis <- df


################################################################################
##  5. Join data.
################################################################################

y_axis <- y_axis[,3:7]

colnames(y_axis) <- c("cmd_code", "cmd_desc", "ChinaImports", "WorldImports", "ShareUkImports"  )

df <- merge(x_axis, y_axis, by = c("cmd_code" , "cmd_desc") )


################################################################################
## 6. Plot the risk.
################################################################################


df$Label <- ifelse(df$ShareUkImports > 0.4 | df$ShareWorldExports > 0.4 , df$cmd_desc, NA)  # give a label to products above 40% in both measures

df$Label <- sub(";.*", "", df$Label)

df$Label2 <- ifelse(df$Label == "Feathers and down, prepared", "Feathers and down, prepared", NA)
df$Label <- ifelse(df$Label == "Feathers and down, prepared", NA, df$Label)

risk <- ggplot(data = df, aes(x = ShareWorldExports, y =ShareUkImports)) +
          geom_point(color = "orange") +
          geom_text(aes(label = Label), color = "orange", na.rm = TRUE, size = 3, vjust = -0.7) +
          geom_text(aes(label = Label2), color = "orange", na.rm = TRUE, size = 3, vjust = 0.7) +
          labs(x = "China's share of global exports", y = "China's share of UK imports") +
          scale_y_continuous(labels = scales::percent) +
          scale_x_continuous(labels = scales::percent)
  
p2 <- risk +
        theme_mywebsite()

setwd(output_d)

png("uk_import_reliance_china_v1.png",  width = 2000, height = 1600, res = 300)
print(p2)
dev.off()


################################################################################