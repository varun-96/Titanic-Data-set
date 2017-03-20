importance <- importance(newrandom)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables,Importance), y = Importance,
                          fill = Importance)) + geom_bar(stat = 'identity') +
            geom_text(aes(x = Variables, y = 0.5, label = Rank),
                      colour = 'red') + coord_flip()


