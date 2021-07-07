MPIData = read.csv(file = 'MappingData.csv')

colnames(MPIData)[1] <- "id"

print(MPIData[3, "id"])
