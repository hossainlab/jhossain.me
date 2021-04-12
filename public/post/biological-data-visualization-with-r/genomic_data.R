# Load Packages 
library(tidyverse)
library(gridExtra)

# Set Theme
theme_set(theme_gray(base_size = 18))

# Loading Data 
data <- read.csv("variants_from_assembly.bed", sep="\t",
                 stringsAsFactors = TRUE, header = FALSE, 
                 quote = '')

# Colnames 
names(data) <-  c("chrom", "start", "stop", "name", "size", "strand", 
                  "type", "ref.dist", "query.dist")
# Colnames 
names(data)

# Data Structures
glimpse(data)


# Categorical Data Visualization 
ggplot(data, aes(x=chrom))+
  geom_bar()

# Filtering Data 
data <- filter(data, chrom %in% c(seq(1,22), "X", "Y", "MT"))
ggplot(data, aes(x=chrom))+
  geom_bar()

# Ordering Chrom 
data$chrom <- factor(data$chrom, levels=c(seq(1,22),"X","Y","MT"))
ggplot(data, aes(x=chrom))+
  geom_bar()


ggplot(data, aes(x=chrom))+
  geom_bar()+
  labs(title = "Chromosomes Distributions", 
       x="Chromosomes", 
       y= "Counts")

# Fill by type 
ggplot(data, aes(x=chrom, fill=type))+
  geom_bar()+
  labs(title = "Chromosomes Distributions", 
       x="Chromosomes", 
       y= "Counts")

# Ordering type 
data$type <- factor(data$type, levels = c("Insertion", "Deletion", 
                                          "Expansion", "Contraction"))

ggplot(data, aes(x=chrom, fill=type))+
  geom_bar()+
  labs(title = "Chromosomes Distributions", 
       x="Chromosomes", 
       y= "Counts")+
  guides(fill=guide_legend(title = "Type"))



# Facet Wrap 
ggplot(data, aes(x=chrom, fill=type))+
  geom_bar()+
  labs(title = "Chromosomes Distributions", 
       x="Chromosomes", 
       y= "Counts")+
  guides(fill=guide_legend(title = "Type"))+
  facet_wrap(~type)


# Facet Grid 
ggplot(data, aes(x=chrom, fill=type))+
  geom_bar()+
  labs(title = "Chromosomes Distributions", 
       x="Chromosomes", 
       y= "Counts")+
  guides(fill=guide_legend(title = "Type"))+
  facet_grid(type ~ .)


# Distribution of size 
ggplot(data, aes(x=size,  color=type))+
  geom_histogram(binwidth = 5, bins = 30)


ggplot(data, aes(x=size, fill=type))+
  geom_bar(binwidth=5)+
  xlim(0, 500)


# Scatter Plot 
ggplot(data, aes(x=ref.dist, y=query.dist))+
  geom_point()

ggplot(data, aes(x=ref.dist, y=query.dist, color=type))+
  geom_point()+
  xlim(-500, 500)+
  ylim(-500, 500)

# Boxplot 
ggplot(data, aes(x=type, y=size))+
  geom_boxplot()



ggplot(data, aes(x=type, y=size))+
  geom_boxplot()+
  coord_flip()

# Violin Plot 
ggplot(data, aes(x=type, y=size))+
  geom_violin()

ggplot(data, aes(x=type, y=size))+
  geom_violin()+
  coord_flip()

ggplot(data, aes(x=type, y=size))+
  geom_violin(adjust=0.2)+
  scale_y_log10()+
  coord_flip()



# Desnsity Plot 
ggplot(data, aes(x=size, fill=type))+
  geom_density(alpha=0.5)+
  xlim(0,500)

ggplot(data, aes(x=size, fill=type))+
  geom_density(alpha=0.5)+
  xlim(0,500)+
  facet_wrap(~type)

ggplot(data, aes(x=size, fill=type))+
  geom_density(alpha=0.5)+
  xlim(0,500)+
  facet_grid(type ~ .)

ggplot(data, aes(x=size, fill=type))+
  geom_density(alpha=0.5)+
  xlim(0,500)+
  facet_grid(. ~type)

ggplot(data, aes(x=size, fill=type))+
  geom_density(alpha=0.5)+
  xlim(0,500)+
  facet_grid(type ~chrom)



# Pie Chart 
type_counts <- summary(data$type)
pie(type_counts)

# Arrange Plot 
f1 <- ggplot(data, aes(x=chrom,fill=type))+ 
  geom_bar()+
  labs(title = "Distribution of Chromosomes",
       x="Chromosomes", y="Counts", 
       tag = "A")+
  guides(fill=guide_legend(title = "Type"))

f2 <- ggplot(data, aes(x=size, fill=type))+
  geom_density(alpha=0.5)+
  xlim(0,500)+
  labs(title = "Density Plot of Size", 
       x = "Size", 
       y = "Probability", 
       tag = "B")+
  guides(fill=guide_legend(title = "Type"))


  grid.arrange(f1, f2, nrow=2, ncol=1)
























