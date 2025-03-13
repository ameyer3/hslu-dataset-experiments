microplastics <- read.csv("Marine_Microplastics_WGS84_-7744337591953404204.csv")
head(microplastics, 5)
colnames(microplastics)

microplastics.df <- subset(microplastics, select = -c(Short.Reference, Long.Reference, DOI, Keywords, Organization, Sampling.Method, NCEI.Accession.Number, GlobalID, NCEI.Accession.Link) )
colnames(microplastics.df)
head(microplastics.df, 5)

install.packages("hexbin")
ggplot(microplastics.df, aes(x = Longitude, y = Latitude)) +
  geom_hex(bins = 100) +  # Adjust bins for resolution
  theme_minimal()

ggplot(microplastics.df, aes(x = Longitude, y = Latitude)) +
  geom_point() +
  theme_minimal()
