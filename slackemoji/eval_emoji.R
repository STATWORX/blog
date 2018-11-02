

# settings ----------------------------------------------------------------
# devtools::install_github("STATWORX/helfRlein")

library(data.table)
library(jsonlite)
library(ggplot2)
library(scales)
library(helfRlein)
library(parallel)
library(stopwords)
library(wordcloud)
library(networkD3)

#devtools::install_github("dill/emoGG")
#library(emoGG)
#devtools::install_github("GuangchuangYu/emojifont")
library(emojifont)
library(gridSVG)

#list.emojifonts()
load.emojifont("EmojiOne.ttf")


main_path <- "slackemoji/"
missing_emoji <- paste0(main_path, "emojis/new_emojilist.txt")

json_files <- list.files(paste0(main_path, "Slack_export/"),
                         full.names = TRUE,
                         recursive = TRUE)
mc_cores <- detectCores() - 1


# load emojis -------------------------------------------------------------

## slack emojis
emoji_json <- fromJSON(paste0(main_path, "emojis/emoji_categories.json"))
emoji_dt <- data.table(EMOJI = paste0(":", unlist(emoji_json), ":"),
                       CATEGORY = rep(names(emoji_json), lapply(emoji_json, length)))

## custom emojis
if (file.exists(paste0(main_path, "emojis/custom_emoji.csv"))) {
  custom_emoji <- as.data.table(read.csv2(paste0(main_path, "emojis/custom_emoji.csv"),
                                          header = FALSE,
                                          stringsAsFactors = FALSE))
  custom_emoji[, INDEX := rep(1:(.N/3), each = 3)]
  custom_emoji <- dcast.data.table(custom_emoji, INDEX ~ V1, value.var = "V2")
} else {
  custom_emoji <- data.table(INDEX = 1,
                             CREATOR = "Jakob",
                             EMOJI = ":nullproblemo:",
                             NAME = "nullproblemo")
}

emoji_list <- unique(c(emoji_dt[, EMOJI], custom_emoji[, EMOJI]))
#emoji_list <- sort(readLines(paste0(main_path, "emojilist.txt")))
#emoji_list <- sort(c(":derkundehatwieder:", ":joy:", ":wink:", ":champagne:", ":100:"))
num_emoji <- length(emoji_list)

emoji_cat <- rbind(custom_emoji[, list(EMOJI = EMOJI, CATEGORY = "custom")],
                   emoji_dt)

# functions ---------------------------------------------------------------

count_reactions <- function(x, reactions, time) {
  #x <- 3
  if (is.null(reactions[[x]]) || is.na(reactions[[x]])) {
    out <- NULL
  } else {
    out <- as.data.table(reactions[[x]])
    out <- out[, list(EMOJI = paste0(":", name, ":"),
                                     REACTION = count)]
    out[, TIME := time[[x]]]
  }
  return(out)
}

count_emoji <- function(x, word, time) {
  # x <- emoji_list
  # word <- word_dt
  # time <- time_dt
  
  # out <- data.table(EMOJI = rep(x, each = length(word)),
  #            COUNT = unlist(sapply(x, function(xi) {
  #              # xi <- x[1]
  #              sapply(word, function(w) {sum(grepl(xi, w))})},
  #              simplify = FALSE)),
  #            TIME = time)
  # 
  reps <- sapply(word, length)
  out <- data.table(EMOJI = rep(x, each = sum(reps)),
                    COUNT = unlist(sapply(x, grepl, x = unlist(word), simplify = FALSE)),
                    TIME = rep(time, reps))
  out <- out[, list(COUNT = sum(COUNT)), by = c("EMOJI", "TIME")]
  
  return(out)
}

add_to_matrix <- function(x, y, mat, emoji_list) {
  # mat <- emoji_net_raw
  # x <- word_dt[[13]]
  # y <- tmp_reactions[[13]]
  if (is.null(y) || is.na(y)) {
    return(mat)
  }
  used_row <- which(emoji_list %in% x)
  if (length(used_row) == 0) {
    return(mat)
  }
  reaction <- y[, REACTION]
  used_col <- which(emoji_list %in% y[, EMOJI])
  if (length(reaction) > length(used_col)) {
    idx <- which( y[, EMOJI] %in% emoji_list)
    out <- ifelse(length(idx) == 0, y[, EMOJI], y[-idx, EMOJI])
    write(out,
          file = missing_emoji,
          sep = "\n",
          append = TRUE) 
    reaction <- reaction[idx]
  }
  
  mat[used_row, used_col] <-  matrix(as.integer(rep(reaction, length(used_row))),
                                     nrow = length(used_row),
                                     byrow = TRUE)
  
  return(mat)
}

eval_json <- function(this_json) {
  #this_json <- json_files[1]
  this_channel <- basename(dirname(this_json))
  daten <- fromJSON(this_json)
  
  time_dt <- as.POSIXct(as.numeric(daten$ts), origin = "1970-01-01", tz = "CET")
  # adjust text
  word_dt <- strsplit(daten$text, " ")
  word_dt <- lapply(word_dt, function(w) {
    unlist(strsplit(w, split = c(":", ":"), type = "between"))
  })
  
  if (length(unlist(word_dt)) == 0) {
    word_dt <- list("")
  }
  
  # adjust reactions ----
  reactions_list <- daten$reactions
  if (is.null(reactions_list)) {
    reactions_list <- as.list(rep(NA, length(word_dt)))
  }
  tmp_reactions <- lapply(seq_along(reactions_list),
                          count_reactions,
                          reactions = reactions_list,
                          time = time_dt)
  reactions <- rbindlist(tmp_reactions)
  if (nrow(reactions) > 0) {
    reactions[, CHANNEL := this_channel]
  }
  
  
  meassages <- count_emoji(x = emoji_list, word = word_dt, time = time_dt)
  meassages[, CHANNEL := this_channel]
  
  # which meassage and reactions come together
  emoji_net_tmp <- lapply(seq_along(word_dt), function(i) {
    # i <- 1
    add_to_matrix(x = word_dt[[i]], y = tmp_reactions[[i]],
                  mat = emoji_net_raw, emoji_list = emoji_list)
  })
  emoji_net <- Reduce("+", emoji_net_tmp)
  
  # count no emojis
  
  
  # tmp_count <- sapply(emoji_list, function(i) grepl(pattern = i, x = word_dt))
  # if (is.null(dim(tmp_count))) {
  #   tmp_count <- matrix(tmp_count, nrow = 1)
  # }
  
  # 1) no emoji within message
  # 2) no emoji as reaction
  # 3) no emoji in both
  # 4) number of messages
  
  
  no_emoji <- c(meassages[, sum(COUNT), by = TIME][, sum(V1 == 0)],
                ifelse(length(tmp_reactions) == 0,
                       0, sum(sapply(tmp_reactions, is.null))),
                ifelse(length(tmp_reactions) == 0,
                       0, sum(sapply(tmp_reactions, is.null) *
                                (meassages[, sum(COUNT) == 0, by = TIME][, V1]))),
                length(reactions_list))

  if (length(tmp_reactions) == 0) {
    tmp_reaction_list <- FALSE
  } else {
    tmp_reaction_list <- !sapply(tmp_reactions, is.null)
    tmp_reaction_list <- rep(tmp_reaction_list, sapply(word_dt, length))
  }
  
  all_words <- data.table(WORD = unlist(word_dt),
                          REACTION = tmp_reaction_list)
  
  out <- list(reactions = reactions,
              meassages = meassages,
              no_emoji = no_emoji,
              emoji_net = emoji_net,
              all_words = all_words)
  
  return(out)
}

get_unicode <-function(x) {
  # x <- plot_data[, EMOJI]
  out <- sapply(x, function(xi) emoji(gsub(":", "", xi)))
  
  # replace emoji NAs with "?"
  if (any(is.na(out))) {
    warning(paste0("changing ", sum(is.na(out))), " missing unicodes to '?'")
    out[is.na(out)] <- emoji("question")
  }
  return(out)
}

# data --------------------------------------------------------------------


# matrix for network: message -> reactions
emoji_net_raw <- emoji_net <- matrix(
  as.integer(0), nrow  = num_emoji, ncol = num_emoji,
  dimnames = list(emoji_list, emoji_list))

# for (i in json_files[320:340]) {
#   # i <- json_files[30]
#   print(which(json_files ==i))
#   eval_json(i)
# }

# this takes a wihle...
result <- mclapply(X = json_files,
                   FUN = eval_json,
                   mc.cores = mc_cores)

# check for missing emojis
if (file.exists(missing_emoji)) {
  new_emoji_list <- unique(sort(readLines(missing_emoji)))
  warning(paste0("these emojis are missing: ",
                 paste0(new_emoji_list, collapse = "  ")))
}

# no emoji percentage
no_emoji <- Reduce("+", lapply(result, "[[", "no_emoji"))
no_emoji_txt <- paste0(((1 - round(no_emoji / no_emoji[4], 2)) * 100)[c(1:3)], "%")
names(no_emoji_txt) <- c("message without emoji",
                         "reaction without emoji",
                         "message and reaction without emoji")
print(t(t(no_emoji_txt)))

# combine all meassages
text_all <- rbindlist(lapply(result, "[[", "meassages"), fill = TRUE)
text_count_all <- text_all[, list(COUNT = sum(COUNT)), by = EMOJI]

# combie all words
all_words <- rbindlist(lapply(result, "[[", "all_words"), fill = TRUE)
all_words[, EMOJI := FALSE]
all_words[WORD %in% emoji_list, EMOJI := TRUE]

all_words[, table(EMOJI)]

all_words[EMOJI == FALSE, WORD := tolower(gsub("[[:punct:]]","",WORD))]
all_words <- all_words[, list(N = .N), by = c("WORD", "REACTION", "EMOJI")]
all_words[, COLOR := ifelse(REACTION, sci_palette()[5], sci_palette()[1])]

## remove stopwords and users
all_words <- all_words[!WORD %in% stopwords("german")
                       & !WORD %in% stopwords("english")
                       & !WORD %in% grep("^u[[:digit:]{1}]", all_words$WORD, value = TRUE)
                       & WORD != ""  & EMOJI == FALSE, ]

#all_words[, table(EMOJI)]
png(filename = paste0(main_path, "plots/wordcloud-noemoji.png"),
    width = 400, height = 300, units = "px")
wordcloud(words = all_words[REACTION == FALSE, ]$WORD,
          freq = all_words[REACTION == FALSE, ]$N,
          max.words = 100,
          min.freq = 10,
          random.order = FALSE,
          colors = all_words[REACTION == FALSE, ]$COLOR,
          ordered.colors = TRUE,
          rot.per = 0,
          fixed.asp = FALSE)
dev.off()

png(filename = paste0(main_path, "plots/wordcloud-emoji.png"),
    width = 400, height = 300, units = "px")
wordcloud(words = all_words[REACTION == TRUE, ]$WORD,
          freq = all_words[REACTION == TRUE, ]$N,
          max.words = 100,
          min.freq = 10,
          random.order = FALSE,
          colors = all_words[REACTION == TRUE, ]$COLOR,
          ordered.colors = TRUE,
          rot.per = 0,
          fixed.asp = FALSE)
dev.off()

png(filename = paste0(main_path, "plots/wordcloud.png"),
    width = 400, height = 300, units = "px")
wordcloud(words = all_words$WORD,
          freq = all_words$N,
          max.words = 100,
          min.freq = 10,
          random.order = FALSE,
          colors = all_words$COLOR,
          ordered.colors = TRUE,
          rot.per = 0,
          fixed.asp = FALSE)
dev.off()


# combine all reactions
reactions_all <- rbindlist(lapply(result, "[[", "reactions"), fill = TRUE)
reactions_count_all <- reactions_all[, list(REACTION = sum(REACTION)), by = EMOJI]

count_all <- merge(reactions_count_all, text_count_all, by = "EMOJI", all = TRUE)
count_all[is.na(COUNT), COUNT := 0]
count_all[is.na(REACTION), REACTION := 0]
count_all[, SUM := REACTION + COUNT]


# count_all[order(COUNT, REACTION, decreasing = TRUE)][1:10,]
# count_all[order(REACTION, COUNT, decreasing = TRUE)][1:10,]
# count_all[order(SUM, COUNT, REACTION, decreasing = TRUE)][1:10,]

# combine with time
text_all[, TYPE := "message"]
reactions_all[, TYPE := "reaction"]
setnames(reactions_all, c("REACTION"), c("COUNT"))


# plots -------------------------------------------------------------------
emoji_time_dt <- rbind(text_all, reactions_all)
emoji_time_dt <- emoji_time_dt[COUNT > 0, ]
emoji_time_dt[, DATE := as.IDate(TIME)]
emoji_time_dt[, TIME := as.ITime(TIME)]
agg_time <- 30 # in minutes
emoji_time_dt[, TIME_AGG := as.ITime(as.numeric(TIME) %/% (60*agg_time) * (60*agg_time))]
emoji_time_dt[, TIME_AGG := as.POSIXct(strptime(TIME_AGG, format="%H:%M:%S"))]

# filter datae before october 2018
emoji_time_dt <- emoji_time_dt[DATE < "2018-10-01",]

emoji_time_dt_agg <- emoji_time_dt[, list(COUNT = sum(COUNT)), by = c("DATE")]
# adjust the time from oct 2017 to sep 2018
emoji_time_dt_agg[,
  MONTH := factor(month(DATE),
                  levels = c(10:12,1:9),
                  labels = c("Jan","Feb","Mar","Apr","May","Jun",
                             "Jul","Aug","Sep 18","Oct 17","Nov","Dec")[c(10:12,1:9)],
                  ordered = TRUE)]
# adjust the week day from Mon to Sun
emoji_time_dt_agg[,
                  WDAY := factor(wday(DATE),
                                  levels = c(2:7,1),
                                  labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"),
                                  ordered = TRUE)]
emoji_time_dt_agg[, MDAY := mday(DATE)]
emoji_time_dt_agg[, YEAR := year(DATE)]
emoji_time_dt_agg[, WEEK := week(DATE)]
emoji_time_dt_agg[, MWEEK := MDAY %/% 7 + 1]


e1 <- ggplot(emoji_time_dt_agg, aes(WEEK, WDAY, fill = COUNT)) + 
  geom_tile(colour = "white") +
  facet_grid( ~ MONTH, scales = "free_x") +
  scale_fill_gradient(low = "white", high = "steelblue",
                      name = "# emojis") +
  theme_bw() +
  # Format the grid
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks=element_blank()) +
  xlab("calendar week") +
  ylab("weekday") +
  theme(axis.text.x=element_text(size=10,color="black")) +
  theme(axis.text.y=element_text(size=10,color="black")) +
  theme(axis.title.x=element_text(size=16,color="black", face = "bold")) +
  theme(axis.title.y=element_text(size=16,color="black", vjust=1.25))
  
ggsave(e1, filename = paste0(main_path, "plots/emoji-over-year.png"),
       width = 16, height = 4, units = c("in"))
       
       


## usage wihtin a day
dt_breaks <- data.table(
  TIME_AGG = as.POSIXct(as.ITime(c("06:00", "09:00", "12:30", "13:30", "18:00"))),
  LABEL = c("wake up!", "opening", "lunchbreak", "", "closing")
  )

emoji_time_dt_agg <- emoji_time_dt[, list(COUNT = sum(COUNT)), by = c("TIME_AGG", "TYPE")]
e2 <- ggplot(emoji_time_dt_agg, aes(x = TIME_AGG, y = COUNT)) +
  geom_bar(stat="identity", fill = "steelblue") +
  scale_x_datetime(labels = date_format("%H:%M")#) +
                   ,limits = as.POSIXct(as.ITime(c("0:0:0","23:59:59")))) +
  geom_vline(dt_breaks, mapping = aes(xintercept = TIME_AGG), color = "black") +
  geom_text(data = dt_breaks, mapping = aes(x = TIME_AGG, y = Inf, label = LABEL),
            angle = 270, vjust = -0.4, hjust = 0, inherit.aes = FALSE) +
  xlab("Time of day") +
  ylab("Number of emojis") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()
ggsave(e2, filename = paste0(main_path, "plots/emoji-over-day.png"),
       width = 8, height = 4, units = c("in"))
  

### categories
# welche Kategorien werden am häufigsten verwendet
emoji_cat[, NUMBER := uniqueN(EMOJI), by = CATEGORY]
emoji_cat_dt <- merge(emoji_cat, emoji_time_dt, by = "EMOJI")
emoji_cat_dt <- emoji_cat_dt[, list(COUNT = sum(COUNT),
                                    UNIQUE = uniqueN(EMOJI)),
                             by = c("CATEGORY", "NUMBER")] # "CHANNEL"
emoji_cat_dt[, DENSITY := round(UNIQUE / NUMBER,2)]
y_lim <- floor(emoji_cat_dt[, range(COUNT)] * c(0,1.1))

e3 <- ggplot(emoji_cat_dt, aes(x = reorder(CATEGORY, -COUNT), y = COUNT, fill = DENSITY)) +
  geom_bar(stat = "identity")+ #, fill = "steelblue") +
  geom_text(aes(label = DENSITY), hjust = -0.1, color = "black", size=3.5) +
  scale_fill_continuous(low = "lightblue", high = "steelblue",
                        name = "emoji density") +
  #scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  xlab("Category") +
  ylab("Number of emojis") +
  ylim(y_lim) +
  coord_flip()
  
ggsave(e3, filename = paste0(main_path, "plots/emoji-category.png"),
       width = 8, height = 4, units = c("in"))

# as REACTION vs within TEXT
top_num <- 50
top_count <- count_all[SUM > 0,][
  order(COUNT, SUM, decreasing = TRUE)][1:top_num, EMOJI]
top_react <- count_all[SUM > 0,][
  order(REACTION, SUM, decreasing = TRUE)][1:top_num, EMOJI]

top_emoji <- unique(c(top_count, top_react))

plot_data <- count_all[EMOJI %in% top_emoji,]
plot_data <- merge(emoji_cat, plot_data, by = "EMOJI")

### normal plots

# REACTION vs COUNT
'
ggplot(plot_data, aes(x = COUNT, y = REACTION)) +
  geom_point() +
  xlab("times used within message") +
  ylab("times used as reaction") +
  #scale_color_brewer(palette = "Paired") +
  theme_minimal()
'

plot_data[, UNICODE := get_unicode(EMOJI)]


quartz()  
# top 50
ggplot(plot_data, aes( x = REACTION, y = COUNT,label = UNICODE)) +
  #geom_point() +
  geom_text(family="EmojiOne") +
  xlab("as reaction") +
  ylab("within message") +
  #xlim(c(0,50)) +
  #ylim(c(0,25))+
  theme_minimal()
ps = grid.export(paste0(main_path, "plots/emoji-plot.svg"), addClass=T)

# top 50 withput top ones
ggplot(plot_data, aes( x = REACTION, y = COUNT, label = UNICODE)) +
  #geom_point() +
  geom_text(family="EmojiOne") +
  xlab("as reaction") +
  ylab("within message") +
  xlim(c(0,120)) +
  ylim(c(0,30))+
  theme_minimal()
ps = grid.export(paste0(main_path, "plots/emoji-plotdetails.svg"), addClass=T)

# time of day with emojis
emoji_time_dt_agg <- copy(emoji_time_dt)
emoji_time_dt_agg[, MAX_COUNT := max(COUNT) == COUNT, by = c("TIME_AGG")]
emoji_time_dt_agg[, SUM_COUNT := sum(COUNT), by = c("TIME_AGG")]
emoji_time_dt_agg[, INDEX := 1:.N, by = c("TIME_AGG")]
emoji_time_dt_agg <- emoji_time_dt_agg[MAX_COUNT == TRUE, ]
emoji_time_dt_agg[, MIN_INDEX := min(INDEX) == INDEX, by = c("TIME_AGG")]
emoji_time_dt_agg <- emoji_time_dt_agg[MIN_INDEX == TRUE, ]
emoji_time_dt_agg[, UNICODE := get_unicode(EMOJI)]

ggplot(emoji_time_dt_agg, aes(x = TIME_AGG, y = SUM_COUNT, label = UNICODE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_x_datetime(labels = date_format("%H:%M")#) +
                   ,limits = as.POSIXct(as.ITime(c("0:0:0","23:59:59")))) +
  geom_vline(dt_breaks, mapping = aes(xintercept = TIME_AGG), color = "black") +
  geom_text(data = dt_breaks, mapping = aes(x = TIME_AGG, y = Inf, label = LABEL),
            angle = 270, vjust = -0.4, hjust = 0, inherit.aes = FALSE) +
  geom_text(family="EmojiOne") +
  xlab("Time of day") +
  ylab("Number of emojis") +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal()
ps = grid.export(paste0(main_path, "plots/emoji-over-day-with-icon.svg"), addClass=T)

dev.off()


### emoji network
# text -> reactions
emoji_net <- Reduce("+", lapply(result, "[[", "emoji_net"))

zero <- 50
non_zero_row <- which(rowSums(emoji_net) > zero)
non_zero_col <- which(colSums(emoji_net) > zero)
non_zero <- sort(unique(c(non_zero_col, non_zero_row)))

net <- emoji_net[non_zero, non_zero]
# only top emojis
# net <- emoji_net[intersect(top_emoji, rownames(emoji_net)),
#                 intersect(top_emoji, colnames(emoji_net))]


DT_net <- data.table(MESSAGE = rownames(emoji_net),
                     REACTION = rep(colnames(emoji_net), each = nrow(emoji_net)),
                     COUNT    = as.numeric(matrix(emoji_net, ncol = 1)))

DT_net <- merge(DT_net, emoji_cat, by.x = "MESSAGE", by.y = "EMOJI")
DT_net <- merge(DT_net, emoji_cat, by.x = "REACTION", by.y = "EMOJI", suffixes = c("", "_REA"))


order_message <- DT_net[order(COUNT, decreasing = TRUE)][, unique(CATEGORY)]
order_reaction <- DT_net[order(COUNT, decreasing = TRUE)][, unique(CATEGORY_REA)]
DT_net[, CATEGORY := factor(CATEGORY, levels = order_message)]
DT_net[, CATEGORY_REA := factor(CATEGORY_REA, levels = order_message)]#rev(order_reaction))]

DT_net <- DT_net[, list(COUNT = sum(COUNT)), by = c("CATEGORY", "CATEGORY_REA")]
DT_net[COUNT == 0, COUNT := NA]

# ggplot(DT_net[COUNT != 0,], aes(x = CATEGORY, y = CATEGORY_REA, size = COUNT)) +
#   geom_point() +
#   theme(axis.text.x  = element_text(angle = 90))

e5 <- ggplot(DT_net, aes(CATEGORY, CATEGORY_REA, fill = COUNT)) + 
  geom_tile(colour = "white") +
  #facet_grid( ~ MONTH, scales = "free_x") +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      name = "# emojis", na.value = "white") +
  theme_bw() +
  # Format the grid
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks=element_blank()) +
  xlab("within message") +
  ylab("as reaction") +
  theme(axis.text.x=element_text(size=10,color="black", angle = 90)) +
  theme(axis.text.y=element_text(size=10,color="black")) +
  theme(axis.title.x=element_text(size=16,color="black", face = "bold")) +
  theme(axis.title.y=element_text(size=16,color="black", vjust=1.25))


# network with categories
net_cat <- list()
net_cat$links <- as.data.frame(
  DT_net[COUNT >0, list(CATEGORY = as.numeric(CATEGORY) -1,
                        CATEGORY_REA = as.numeric(CATEGORY_REA) - 1 + uniqueN(CATEGORY),
                        COUNT = COUNT)])
net_cat$nodes <- as.data.frame(DT_net[, list(name =  c(levels(CATEGORY),
                                                       levels(CATEGORY_REA)))])

# Plot
s1 <- sankeyNetwork(Links = net_cat$links, Nodes = net_cat$nodes,
                    Source = "CATEGORY", Target = "CATEGORY_REA",
                    Value = "COUNT", NodeID = "name",
                    units = "count", fontSize = 12, nodeWidth = 30)
# did not work with just the main_path
html_save <- file.path(paste0(getwd(), "/", main_path,
                              "plots/emoji-network-category.html"))
saveNetwork(s1, file = html_save,
            selfcontained = FALSE)


