
library(dplyr)



rows <- 1:5
cols <- 1:6

edges <- list()
k <- 1
for (row in rows) {
   for (col in cols) {
     src <- sprintf("%s%d", LETTERS[row], col)

     for (c in (col + c(-1, 1))) {
       if (c >= 1 && c <= max(cols)) {
         edges[[k]] <- data.frame(src = src,
           dst = sprintf("%s%d", LETTERS[row], c))
         k <- k + 1
       }
     }

     for (r in (row + c(-1, 1))) {
       if (r >= 1 && r <= max(rows)) {
         edges[[k]] <- data.frame(src = src,
           dst = sprintf("%s%d", LETTERS[r], col))
         k <- k + 1
       }
     }
   }
}

edges <- bind_rows(edges)


vertices <- expand.grid(row = LETTERS[1:5], col = 1:6) %>%
   mutate(id = sprintf("%s%d", row, col)) %>%
   select(id) %>% mutate(color = "pink")

purple <- c("B1", "B2", "B5", "C1", "C2", "C5", "D1", "D2", "D3", "D4") 
purple <- c("B1", "B2", "B5", "C1", "C2", "D1", "D2", "D3", "D4") 
vertices$color[vertices$id %in% purple] <- "purple"
vertices


source("R/ssi.R")


devtools::load_all("./")

ssi(vertices, edges, group = "pink")

