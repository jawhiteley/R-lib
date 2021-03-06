# Drop version info, which doesn't help us.
depends$LinkedPackage <- sub('\\s*\\(.*', '', as.character(depends$LinkedPackage))

suggests$LinkedPackage <- sub('\\s*\\(.*', '', as.character(suggests$LinkedPackage))

imports$LinkedPackage <- sub('\\s*\\(.*', '', as.character(imports$LinkedPackage))

for (user in unique(installations$User))
{
  installations <- rbind(installations, data.frame(Package = "R",
                                                   Version = "2.10.1",
                                                   User = user))
}

user.count <- with(installations, length(unique(User)))

packages <- transform(packages,
                      InstallProbability = sapply(packages$Package,
                                                  function (p)
                                                  {
                                                    nrow(subset(installations,
                                                                Package == as.character(p))) / user.count
                                                  }))
