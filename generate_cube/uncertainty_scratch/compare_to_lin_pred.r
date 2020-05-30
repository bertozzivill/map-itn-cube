
library(data.table)
library(ggplot2)

test_pred <- fread("~/Desktop/full_predictions_ben_2012_means.csv")

data_pred <- fread("~/Desktop/access_dev_preds.csv")
data_pred <- data_pred[iso3=="BEN" & year==2012]

compare <- merge(data_pred, test_pred[metric=="emp_access_dev", list(iso3, month, cellnumber, full)])
ggplot(compare, aes(x=pred_ihs_emp_access_dev, y=full)) + geom_abline() +  geom_point()
