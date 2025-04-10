library(readr)
library(tidyverse)
w_perc <- read_csv("../../db/mlb/cle.txt")
str(w_perc)
w_perc <- rename(w_perc, year = Year, perc = 'W-L%', team=Tm)
#w_perc <- select(w_perc, year, perc)
w_perc <- filter(w_perc, year >= 1925, year < 2025)

cat("../../db/mlb/",get[ana],".txt")
city <- "ana"
paste0("../../db/mlb/",city,'.txt')

perc_win <- function(team = "str"){
  txtfile <- paste0("../../db/mlb/", team,".txt")
  sheet <- read_csv(txtfile)
  sheet <- sheet %>%
    rename(year = Year, perc = 'W-L%', team=Tm) %>%
    select(year, team, perc) %>%
    filter(year >= 1925, year < 2025)
  return(sheet)
}

cleveland <- perc_win("cle")
head(cleveland)

teams <- list("ana", "ari", "atl", "bal", "bos", "chc", "chw", "cin", "cle", "col", "det", "hou", "kcr", "lad", "mia", "mil", "min", "nym", "nyy", "oak", "phi", "pit", "sdp", "sea", "sfg", "stl", "tbd", "tex", "tor", "wsn")
length(teams)

win_perc <- data.frame()
for(i in teams){
  data = perc_win(i)
  win_perc <- bind_rows(win_perc, data)
}

g <- ggplot(win_perc, aes(x=perc))
g + geom_histogram(bins=100, color="black", fill="red") + xlim(0, 1)

g + boxplot(win_perc$perc)

quantile(win_perc$perc, probs = c(0.1, 0.9))
min(win_perc$perc)
max(win_perc$perc)

filter(win_perc, perc == 0.721)
filter(win_perc, perc == 0.248)


rmarkdown::render("project_2.Rmd", 
                  output_file = "README.md", 
                  output_format = "github_document",
                  output_options = list(toc=TRUE, toc_depth=1,
                                        number_sections=TRUE, df_print="default"))