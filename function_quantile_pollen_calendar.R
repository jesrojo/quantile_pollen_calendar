library(AeRobiology)
library(scales)
library(tidyverse)
library(RColorBrewer)

quantile.pollen.calendar <- function(df, y1, y2, q = c(80, 90, 92.5, 95, 97.5, 99, 99.5), p = 90, lg = 4, inpntype = 16){  
  abun.ord <- iplot_abundance(df, interpolation = F, n.types = as.numeric(length(unique(colnames(df)))))$data
  typesel <- as.vector(abun.ord$types[order(abun.ord$mean, decreasing = T)][1:inpntype])
  
  levels <- round(data.frame(apply(df[which(as.numeric(strftime(df$date, "%Y")) %in% y1:y2) ,colnames(df) %in% typesel], 2, FUN = function(x) quantile(x, p = q/100, na.rm = T))))
  
  typesel <- names(which(colSums(apply(levels, 2, FUN = function(x) duplicated(x))) == 0)) #Remove with very low pollen load

  df <- df[which(as.numeric(strftime(df$date, "%Y")) %in% y1:y2), ]
  
  quantile.df <- data.frame()
  dat <- df
  dat$jd <- as.numeric(strftime(dat$date, "%j"))
  dat <- dat[-which(dat$jd == 366), ]
  
  q.df <- data.frame(jd = 1:365)
  for(t in typesel){
    q.vc <- numeric()
    for (i in 1:365){
      range <- (i-lg):(i+lg)
      range[which(range <= 0)] <- range[which(range <= 0)] + 365
      range[which(range > 365)] <- range[which(range > 365)] - 365
      
      q.vc <- c(q.vc, quantile(dat[dat$jd %in% range, t], probs = p/100, na.rm = T))
    }
    q.df[ ,t] <- q.vc
  }
  quantile.df <- rbind(quantile.df, q.df)

  
  ####################################################################
  ####################################################################
  
  cal.df <- data.frame(jd = quantile.df$jd)
  for(t in typesel){
    cal.df[ ,t] <- cut(quantile.df[ ,t], breaks = c(-Inf, levels[ ,t], Inf), labels = 1:(nrow(levels)+1))
  }
  
  cal.df <- gather(cal.df, key = type, value = class, - jd)
  
  or.df <- data.frame(type = typesel, or = NA)
  for(t in typesel){
    or.df[which(or.df == t), "or"] <- cal.df$jd[which(cal.df$type == t & as.numeric(cal.df$class[which(cal.df$type == t)]) == max(as.numeric(cal.df$class[which(cal.df$type == t)])))][1]
  }
  
  cal.df$jd <- as.Date(strptime(cal.df$jd, "%j"))
  
  ####################################################################
  ####################################################################
  
  print(levels)
  
  ggplot(data = cal.df, aes(x = jd, y = factor(type, levels = or.df$type[order(or.df$or, decreasing = T)]), fill = class)) +
    geom_tile() +
    geom_hline(yintercept = seq(1.5, 14.5, 1), linetype = 3, colour = "gray50", size = 0.1) +
    scale_fill_manual(name = "Level", values = c("gray95", brewer.pal(length(row.names(levels)), "YlOrRd")), labels = c("null", row.names(levels))) +
    scale_x_date(breaks = "1 month", labels = date_format("%b"), expand = c(0, 0)) +
    guides(fill = guide_legend(nrow = 1)) +
    theme_bw() +
    theme(plot.title = element_text(size = 12, face = "bold"),
          legend.position = "bottom",
          legend.title = element_text(size = 10),
          axis.text.y = element_text(face = "italic", size = 9),
          axis.title = element_blank(),
          strip.text = element_text(size = 12, face = "bold"))
  

  
}  



  
